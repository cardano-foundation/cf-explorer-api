package com.cardano.explorer.entity;

import java.sql.Timestamp;
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
@Table(name = "meta", uniqueConstraints = {
    @UniqueConstraint(name = "unique_meta",
        columnNames = {"start_time"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class Meta extends BaseEntity {

  @Column(name = "start_time", nullable = false)
  private Timestamp startTime;

  @Column(name = "network_name", nullable = false)
  private String networkName;

  @Column(name = "version", nullable = false)
  private String version;

}
