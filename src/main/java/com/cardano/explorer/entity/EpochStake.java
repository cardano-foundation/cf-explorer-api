package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Lovelace;
import com.cardano.explorer.validation.Word31Type;
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
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@Entity
@Table(name = "epoch_stake", uniqueConstraints = {
    @UniqueConstraint(name = "unique_stake",
        columnNames = {"epoch_no", "addr_id", "pool_id"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
@org.hibernate.annotations.Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class EpochStake extends BaseEntity {

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "addr_id", nullable = false,
      foreignKey = @ForeignKey(name = "epoch_stake_addr_id_fkey"))
  @EqualsAndHashCode.Exclude
  private StakeAddress addr;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "pool_id", nullable = false,
      foreignKey = @ForeignKey(name = "epoch_stake_pool_id_fkey"))
  @EqualsAndHashCode.Exclude
  private PoolHash pool;

  @Column(name = "amount", nullable = false)
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal amount;

  @Column(name = "epoch_no", nullable = false)
  @Word31Type
  private Integer epochNo;

}
