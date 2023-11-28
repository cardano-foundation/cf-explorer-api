package org.cardanofoundation.explorer.api.model.response.pool;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.api.common.enumeration.PoolActionType;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class TxPoolCertificateHistory extends PoolCertificateHistory {
  List<PoolActionType> actions;
}