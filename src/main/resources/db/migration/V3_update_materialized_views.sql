drop materialized view sanchonet.tx_chart;

create materialized view sanchonet.tx_chart as
SELECT date_part('epoch'::text, date_trunc('minute'::text, simple_tx_tmp.tx_time)) AS minute,
       date_part('epoch'::text, date_trunc('hour'::text, simple_tx_tmp.tx_time))   AS hour,
       date_part('epoch'::text, date_trunc('day'::text, simple_tx_tmp.tx_time))    AS day,
       date_part('epoch'::text, date_trunc('month'::text, simple_tx_tmp.tx_time))  AS month,
       date_part('epoch'::text, date_trunc('year'::text, simple_tx_tmp.tx_time))   AS year,
       count(simple_tx_tmp.tx_id)                                                  AS tx_count,
       sum(
               CASE
                   WHEN simple_tx_tmp.tx_with_sc = true THEN 1
                   ELSE 0
                   END)                                                            AS tx_with_sc,
       sum(
               CASE
                   WHEN simple_tx_tmp.tx_with_metadata_without_sc = true THEN 1
                   ELSE 0
                   END)                                                            AS tx_with_metadata_without_sc,
       sum(
               CASE
                   WHEN simple_tx_tmp.simple_tx = true THEN 1
                   ELSE 0
                   END)                                                            AS tx_simple
FROM (SELECT tx.id                 AS tx_id,
    b."time"              AS tx_time,
    sum(
    CASE
    WHEN r.id IS NOT NULL THEN 1
    ELSE 0
    END) <> 0 AS tx_with_sc,
    sum(
    CASE
    WHEN r.id IS NULL AND tm.id IS NOT NULL THEN 1
    ELSE 0
    END) <> 0 AS tx_with_metadata_without_sc,
    sum(
    CASE
    WHEN r.id IS NULL AND tm.id IS NULL THEN 1
    ELSE 0
    END) <> 0 AS simple_tx
    FROM sanchonet.tx tx
    JOIN sanchonet.block b ON tx.block_id = b.id
    LEFT JOIN sanchonet.redeemer r ON tx.id = r.tx_id
    LEFT JOIN sanchonet.tx_metadata tm ON tx.id = tm.tx_id
    GROUP BY tx.id, b."time") simple_tx_tmp
GROUP BY (date_part('epoch'::text, date_trunc('minute'::text, simple_tx_tmp.tx_time))),
    (date_part('epoch'::text, date_trunc('hour'::text, simple_tx_tmp.tx_time))),
    (date_part('epoch'::text, date_trunc('day'::text, simple_tx_tmp.tx_time))),
    (date_part('epoch'::text, date_trunc('month'::text, simple_tx_tmp.tx_time))),
    (date_part('epoch'::text, date_trunc('year'::text, simple_tx_tmp.tx_time)))
ORDER BY (date_part('epoch'::text, date_trunc('minute'::text, simple_tx_tmp.tx_time)));

alter materialized view sanchonet.tx_chart owner to "cardano-master";

create unique index unique_tx_chart_idx
    on sanchonet.tx_chart (minute, hour, day, month, year);

create index idx_mat_tx_chart_minute
    on sanchonet.tx_chart (minute);

create index idx_mat_tx_chart_hour
    on sanchonet.tx_chart (hour);

create index idx_mat_tx_chart_day
    on sanchonet.tx_chart (day);

create index idx_mat_tx_chart_month
    on sanchonet.tx_chart (month);

