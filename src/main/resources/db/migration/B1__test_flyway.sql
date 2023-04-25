
create type staking_life_cycle_event_type as ENUM('REGISTRATION', 'REWARDS', 'DELEGATION', 'DEREGISTRATION', 'WITHDRAWAL');

create type report_history_status as ENUM('PENDING', 'IN_PROGRESS', 'GENERATED', 'FAILED');

create table if not exists stake_key_report_history(
    id bigserial primary key,
    storage_key character varying(255),
    stake_key character varying(255) not null,
    report_name character varying(255) not null,
    from_date timestamp not null,
    to_date timestamp not null,
    is_ada_transfer boolean default false,
    status report_history_status,
    is_deleted boolean default false,
    constraint unique_storage_key
        unique (storage_key)
);

create cast (character varying as staking_life_cycle_event_type) with inout as IMPLICIT;

create table if not exists staking_life_cycle_event(
    event_type staking_life_cycle_event_type primary key
);

create table if not exists stake_key_report_history_event(
    id bigserial primary key,
    report_history_id bigint not null,
    event_type staking_life_cycle_event_type not null,
    constraint unique_report_history_staking_life_cycle_event
         unique (report_history_id, event_type)
);

create sequence if not exists report_history_id_seq
    start with 1
    increment by 1
    no minvalue
    no maxvalue
    cache 1;

create sequence if not exists stake_key_report_history_id_seq
    start with 1
    increment by 1
    no minvalue
    no maxvalue
    cache 1;

insert into staking_life_cycle_event(event_type)
values ('REGISTRATION'),
        ('REWARDS'),
        ('DELEGATION'),
        ('DEREGISTRATION'),
        ('WITHDRAWAL');