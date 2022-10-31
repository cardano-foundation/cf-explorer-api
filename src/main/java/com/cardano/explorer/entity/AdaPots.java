package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Lovelace;
import com.cardano.explorer.validation.Word31Type;
import com.cardano.explorer.validation.Word63Type;
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
@Table(name = "ada_pots", uniqueConstraints = {
    @UniqueConstraint(name = "unique_ada_pots", columnNames = {"block_id"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class AdaPots extends BaseEntity {

  @Column(name = "slot_no", nullable = false)
  @Word63Type
  private Long slotNo;

  @Column(name = "epoch_no", nullable = false)
  @Word31Type
  private Integer epochNo;

  @Column(name = "treasury", nullable = false)
  @Lovelace
  @Digits(integer=20, fraction=0)
  private BigDecimal treasury;

  @Column(name = "reserves", nullable = false)
  @Lovelace
  @Digits(integer=20, fraction=0)
  private BigDecimal reserves;

  @Column(name = "rewards", nullable = false)
  @Lovelace
  @Digits(integer=20, fraction=0)
  private BigDecimal rewards;

  @Column(name = "utxo", nullable = false)
  @Lovelace
  @Digits(integer=20, fraction=0)
  private BigDecimal utxo;

  @Column(name = "deposits", nullable = false)
  @Lovelace
  @Digits(integer=20, fraction=0)
  private BigDecimal deposits;

  @Column(name = "fees", nullable = false)
  @Lovelace
  @Digits(integer=20, fraction=0)
  private BigDecimal fees;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "block_id", nullable = false, unique = true,
      foreignKey = @ForeignKey(name = "ada_pots_block_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Block block;

}
