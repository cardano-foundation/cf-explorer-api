package com.cardano.explorer.entity;

import com.cardano.explorer.common.enumeration.ScriptPurposeType;
import com.cardano.explorer.validation.Hash28Type;
import com.cardano.explorer.validation.Lovelace;
import com.cardano.explorer.validation.Word31Type;
import com.cardano.explorer.validation.Word63Type;
import java.math.BigDecimal;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
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
@Table(name = "redeemer", uniqueConstraints = {
    @UniqueConstraint(name = "unique_redeemer",
        columnNames = {"tx_id", "purpose", "index"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class Redeemer extends BaseEntity {

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "tx_id", nullable = false,
      foreignKey = @ForeignKey(name = "redeemer_tx_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Tx tx;

  @Column(name = "unit_mem", nullable = false)
  @Word63Type
  private Long unitMem;

  @Column(name = "unit_steps", nullable = false)
  @Word63Type
  private Long unitSteps;

  @Column(name = "fee")
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal announcedTxId;

  @Column(name = "purpose", nullable = false)
  @Enumerated(EnumType.STRING)
  private ScriptPurposeType purpose;

  @Column(name = "index", nullable = false)
  @Word31Type
  private Integer index;

  @Column(name = "script_hash")
  @Hash28Type
  private byte[] scriptHash;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "redeemer_data_id", nullable = false,
      foreignKey = @ForeignKey(name = "redeemer_redeemer_data_id_fkey"))
  @EqualsAndHashCode.Exclude
  private RedeemerData redeemerData;

}
