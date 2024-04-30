package org.cardanofoundation.explorer.api.model.response.drep;

import static org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig.DATE_TIME_FORMAT;

import java.sql.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;

import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.DRepActionType;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class DRepCertificateHistory {
  String txHash;

  @JsonIgnore Long txIndex;

  @JsonIgnore DRepActionType type;

  Long blockNo;

  Long slotNo;

  Integer epochNo;

  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = DATE_TIME_FORMAT)
  Date createdAt;

  Long absoluteSlot;
}
