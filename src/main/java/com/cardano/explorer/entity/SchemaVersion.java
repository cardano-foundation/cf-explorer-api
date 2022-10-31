package com.cardano.explorer.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Entity
@Table(name = "schema_version")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class SchemaVersion extends BaseEntity {

  @Column(name = "stage_one", nullable = false)
  private Long stageOne;

  @Column(name = "stage_two", nullable = false)
  private Long stageTwo;

  @Column(name = "stage_three", nullable = false)
  private Long stageThree;

}
