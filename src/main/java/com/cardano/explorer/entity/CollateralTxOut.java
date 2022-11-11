package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Hash28Type;
import com.cardano.explorer.validation.Hash32Type;
import com.cardano.explorer.validation.Lovelace;
import com.cardano.explorer.validation.TxIndex;
import java.math.BigDecimal;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.Digits;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@Entity
@Table(name = "collateral_tx_out", uniqueConstraints = {
    @UniqueConstraint(name = "unique_col_txout",
        columnNames = {"tx_id", "index"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class CollateralTxOut extends BaseEntity {

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "tx_id", nullable = false,
      foreignKey = @ForeignKey(name = "collateral_tx_out_tx_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Tx tx;

  @Column(name = "index", nullable = false)
  @TxIndex
  private Short index;

  @Column(name = "address", nullable = false)
  private String address;

  @Column(name = "address_raw", nullable = false)
  private byte[] addressRaw;

  @Column(name = "address_has_script", nullable = false)
  private Boolean addressHasScript;

  @Column(name = "payment_cred")
  @Hash28Type
  private byte[] paymentCred;

  @ManyToOne(fetch = FetchType.LAZY)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "stake_address_id",
      foreignKey = @ForeignKey(name = "collateral_tx_out_stake_address_id_fkey"))
  @EqualsAndHashCode.Exclude
  private StakeAddress stakeAddress;

  @Column(name = "value", nullable = false)
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal value;

  @Column(name = "data_hash")
  @Hash32Type
  private byte[] dataHash;

  @Column(name = "multi_assets_descr", nullable = false)
  private String multiAssetsDescr;

  @ManyToOne(fetch = FetchType.LAZY)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "inline_datum_id",
      foreignKey = @ForeignKey(name = "collateral_tx_out_inline_datum_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Datum inlineDatum;

  @ManyToOne(fetch = FetchType.LAZY)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "reference_script_id",
      foreignKey = @ForeignKey(name = "collateral_tx_out_reference_script_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Script referenceScript;

}
