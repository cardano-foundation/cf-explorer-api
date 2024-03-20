package org.cardanofoundation.explorer.api.model.response.drep;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.DRepActionType;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class DRepCertificateHistoryResponse extends DRepCertificateHistory {
  List<DRepActionType> actionTypes;
}
