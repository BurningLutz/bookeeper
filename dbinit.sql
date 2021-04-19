BEGIN;

-- function to update the column `updated_at`
create or replace function trigger_updated_at() returns trigger language plpgsql as $$
begin
  new.updated_at = now();
  return new;
end;
$$;

-- function to set trigger on any tables of schema `core`
create or replace function set_trigger_updated_at() returns event_trigger language plpgsql as $$
declare
  o record;
begin
  for o in select * from pg_event_trigger_ddl_commands()
  loop
    if o.object_type = 'table' and o.command_tag = 'CREATE TABLE' then
      execute 'create trigger set_trigger_updated_at
               before update on ' || o.object_identity || '
               for each row
               execute procedure trigger_updated_at()';
    end if;
  end loop;
end;
$$;

-- trigger to set trigger
create event trigger set_trigger_updated_at
on ddl_command_end
when tag in ('create table')
execute procedure set_trigger_updated_at();


create type borrowing_status as enum ('pending', 'approved', 'returned');


create table base (
  id         bigint      primary key generated always as identity,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create index on base (created_at);
create index on base (updated_at);


create table admins (
  like base including all,

  nickname varchar(20) not null unique,
  password varchar(80) not null
);


create table users (
  like base including all,

  nickname varchar(20) not null unique,
  isVip    bool        not null,
  age      int4
);


create table books (
  like base including all,

  sn     varchar(40) not null unique,
  title  varchar(40) not null,
  author varchar(20) not null
);


create table borrowings (
  like base including all,

  book_id bigint           not null references books (id),
  user_id bigint           not null references users (id),
  date    timestamptz      not null,
  status  borrowing_status not null
);
create index on borrowings (book_id);
create index on borrowings (user_id);

COMMIT;

-- default serializable
alter system set default_transaction_isolation = 'serializable';
-- discard pg_reload_conf return value
do language plpgsql $$
begin
  perform pg_reload_conf();
end;
$$;
