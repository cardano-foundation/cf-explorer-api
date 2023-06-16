create table agg_address_token
(
    id      bigserial primary key,
    balance numeric(39) not null,
    ident   bigint      not null,
    day     date
);

create table agg_address_tx_balance
(
    id                  bigserial primary key,
    stake_address_id    bigint,
    address_id          bigint      not null,
    balance             numeric(39) not null,
    day                 date        not null,
    current_sum_balance numeric(39),
    current_min_balance numeric(39),
    current_max_balance numeric(39)
);
