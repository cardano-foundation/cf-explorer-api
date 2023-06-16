CREATE INDEX IF NOT EXISTS address_token_ident_tx_id_balance_idx
    ON address_token (ident, tx_id, balance);

CREATE INDEX IF NOT EXISTS agg_address_token_ident_day_balance_idx
    ON agg_address_token (ident, day, balance);

CREATE INDEX IF NOT EXISTS agg_address_tx_balance_stake_address_id_day_balance_index
    ON agg_address_tx_balance (stake_address_id, day, balance);

CREATE INDEX IF NOT EXISTS agg_address_tx_balance_address_id_day_balance_index
    ON agg_address_tx_balance (address_id, day, balance);

CREATE INDEX IF NOT EXISTS agg_address_tx_balance_day_index
    ON agg_address_tx_balance (day desc);
