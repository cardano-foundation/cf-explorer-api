package com.cardano.explorer.entity;

import com.cardano.explorer.common.enumeration.RewardType;
import com.cardano.explorer.validation.Lovelace;
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
@Table(name = "reward", uniqueConstraints = {
    @UniqueConstraint(name = "unique_reward",
        columnNames = {"addr_id", "type", "earned_epoch", "pool_id"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class Reward extends BaseEntity {

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "addr_id", nullable = false,
      foreignKey = @ForeignKey(name = "reward_addr_id_fkey"))
  @EqualsAndHashCode.Exclude
  private StakeAddress addr;

  @Column(name = "type", nullable = false)
  @Enumerated(EnumType.STRING)
  private RewardType type;

  @Column(name = "amount", nullable = false)
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal amount;

  @Column(name = "earned_epoch", nullable = false)
  private Long earnedEpoch;

  @Column(name = "spendable_epoch", nullable = false)
  private Long spendableEpoch;

  @ManyToOne(fetch = FetchType.LAZY)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "pool_id",
      foreignKey = @ForeignKey(name = "reward_pool_id_fkey"))
  @EqualsAndHashCode.Exclude
  private PoolHash pool;

}
