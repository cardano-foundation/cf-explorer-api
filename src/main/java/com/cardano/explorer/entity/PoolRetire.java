package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Word31Type;
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
@Table(name = "pool_retire", uniqueConstraints = {
    @UniqueConstraint(name = "unique_pool_retiring",
        columnNames = {"announced_tx_id", "cert_index"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class PoolRetire extends BaseEntity {

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "hash_id", nullable = false,
      foreignKey = @ForeignKey(name = "pool_retire_hash_id_fkey"))
  private PoolHash poolHash;

  @Column(name = "cert_index", nullable = false)
  private Integer certIndex;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "announced_tx_id", nullable = false,
      foreignKey = @ForeignKey(name = "pool_retire_announced_tx_id_fkey"))
  private Tx announcedTx;

  @Column(name = "retiring_epoch", nullable = false)
  @Word31Type
  private Integer retiringEpoch;


}
