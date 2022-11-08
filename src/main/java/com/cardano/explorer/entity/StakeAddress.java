package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Addr29Type;
import com.cardano.explorer.validation.Hash28Type;
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
@Table(name = "stake_address", uniqueConstraints = {
    @UniqueConstraint(name = "unique_stake_address",
        columnNames = {"hash_raw"}
    )
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class StakeAddress extends BaseEntity {

  @Column(name = "hash_raw", nullable = false)
  @Addr29Type
  private byte[] hashRaw;

  @Column(name = "view", nullable = false)
  private String view;

  @Column(name = "script_hash")
  @Hash28Type
  private byte[] scriptHash;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "tx_id", nullable = false,
      foreignKey = @ForeignKey(name = "stake_address_tx_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Tx tx;

}
