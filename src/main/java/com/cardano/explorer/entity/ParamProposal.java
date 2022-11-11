package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Hash28Type;
import com.cardano.explorer.validation.Hash32Type;
import com.cardano.explorer.validation.Lovelace;
import com.cardano.explorer.validation.Word31Type;
import com.cardano.explorer.validation.Word64Type;
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
@Table(name = "param_proposal", uniqueConstraints = {
    @UniqueConstraint(name = "unique_param_proposal",
        columnNames = {"key", "registered_tx_id"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class ParamProposal extends BaseEntity {

  @Column(name = "epoch_no", nullable = false)
  @Word31Type
  private Integer epochNo;

  @Column(name = "key", nullable = false)
  @Hash28Type
  private byte[] key;

  @Column(name = "min_fee_a")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal minFeeA;

  @Column(name = "min_fee_b")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal minFeeB;

  @Column(name = "max_block_size")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal maxBlockSize;

  @Column(name = "max_tx_size")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal maxTxSize;

  @Column(name = "max_bh_size")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal maxBhSize;

  @Column(name = "key_deposit")
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal keyDeposit;

  @Column(name = "pool_deposit")
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal poolDeposit;

  @Column(name = "max_epoch")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal maxEpoch;

  @Column(name = "optimal_pool_count")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal optimalPoolCount;

  @Column(name = "influence")
  private Double influence;

  @Column(name = "monetary_expand_rate")
  private Double monetaryExpandRate;

  @Column(name = "treasury_growth_rate")
  private Double treasuryGrowthRate;

  @Column(name = "decentralisation")
  private Double decentralisation;

  @Column(name = "entropy")
  @Hash32Type
  private byte[] entropy;

  @Column(name = "protocol_major")
  @Word31Type
  private Integer protocolMajor;

  @Column(name = "protocol_minor")
  @Word31Type
  private Integer protocolMinor;

  @Column(name = "min_utxo_value")
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal minUtxoValue;

  @Column(name = "min_pool_cost")
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal minPoolCost;

  @ManyToOne(fetch = FetchType.LAZY)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "cost_model_id",
      foreignKey = @ForeignKey(name = "param_proposal_cost_model_id_fkey"))
  @EqualsAndHashCode.Exclude
  private CostModel costModel;

  @Column(name = "price_mem")
  private Double priceMem;

  @Column(name = "price_step")
  private Double priceStep;

  @Column(name = "max_tx_ex_mem")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal maxTxExMem;

  @Column(name = "max_tx_ex_steps")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal maxTxExSteps;

  @Column(name = "max_block_ex_mem")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal maxBlockExMem;

  @Column(name = "max_block_ex_steps")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal maxBlockExSteps;

  @Column(name = "max_val_size")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal maxValSize;

  @Column(name = "collateral_percent")
  @Word31Type
  private Integer collateralPercent;

  @Column(name = "max_collateral_inputs")
  @Word31Type
  private Integer maxCollateralInputs;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "registered_tx_id", nullable = false,
      foreignKey = @ForeignKey(name = "param_proposal_registered_tx_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Tx registeredTx;

  @Column(name = "coins_per_utxo_size")
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal coinsPerUtxoSize;

}
