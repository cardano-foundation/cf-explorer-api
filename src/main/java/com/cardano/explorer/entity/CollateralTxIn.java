package com.cardano.explorer.entity;

import com.cardano.explorer.validation.TxIndex;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@Entity
@Table(name = "collateral_tx_in", uniqueConstraints = {
    @UniqueConstraint(name = "unique_col_txin",
        columnNames = {"tx_in_id", "tx_out_id", "tx_out_index"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class CollateralTxIn extends BaseEntity {

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "tx_in_id", nullable = false,
      foreignKey = @ForeignKey(name = "collateral_tx_in_tx_in_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Tx txIn;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "tx_out_id", nullable = false,
      foreignKey = @ForeignKey(name = "collateral_tx_in_tx_out_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Tx txOut;

  @Column(name = "tx_out_index", nullable = false)
  @TxIndex
  private Short txOutIndex;

}
