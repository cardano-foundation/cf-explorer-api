package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Hash28Type;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Entity
@Table(name = "reserved_pool_ticker", uniqueConstraints = {
    @UniqueConstraint(name = "unique_reserved_pool_ticker",
        columnNames = {"name"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class ReservedPoolTicker extends BaseEntity {

  @Column(name = "name", nullable = false)
  private String name;

  @Column(name = "pool_hash", nullable = false)
  @Hash28Type
  private byte[] poolHash;

}
