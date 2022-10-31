package com.cardano.explorer.entity;

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
@Table(name = "pool_relay", uniqueConstraints = {
    @UniqueConstraint(name = "unique_pool_relay",
        columnNames = {"update_id", "ipv4", "ipv6", "dns_name"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class PoolRelay extends BaseEntity {

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "update_id", nullable = false,
      foreignKey = @ForeignKey(name = "pool_relay_update_id_fkey"))
  @EqualsAndHashCode.Exclude
  private PoolUpdate poolUpdate;

  @Column(name = "ipv4", nullable = false)
  private String ipv4;

  @Column(name = "ipv6", nullable = false)
  private String ipv6;

  @Column(name = "dns_name", nullable = false)
  private String dnsName;

  @Column(name = "dns_srv_name", nullable = false)
  private String dnsSrvName;

  @Column(name = "port", nullable = false)
  private Integer port;

}
