package org.cardanofoundation.explorer.api.model.response.pool;

import java.sql.Timestamp;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.cardanofoundation.explorer.api.common.enumeration.PoolActionType;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class PoolCertificateHistory {

  @JsonIgnore
  private Long txId;
  @JsonIgnore
  private Long poolHashId;
  @JsonProperty("epochNo")
  private Integer txEpochNo; // epoch of the tx
  @JsonIgnore
  private Integer certEpochNo; // epoch of the cert
  @JsonIgnore
  private Integer certIndex;
  @JsonIgnore
  private Long poolRetireId;
  @JsonIgnore
  private Long poolUpdateId;
  private String txHash;
  private Timestamp blockTime;
  private Long blockNo;
  private Integer epochSlotNo;
  private Integer slotNo;
  @JsonIgnore
  private PoolActionType actionType;
}