package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Word64Type;
import java.math.BigDecimal;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.JoinColumn;
import javax.persistence.Lob;
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
@Table(name = "tx_metadata", uniqueConstraints = {
    @UniqueConstraint(name = "unique_tx_metadata",
        columnNames = {"key", "tx_id"}
    )
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class TxMetadata extends BaseEntity {

  @Column(name = "key", nullable = false)
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal key;

  @Column(name = "json")
  @Lob
  private String json;

  @Column(name = "bytes", nullable = false)
  private byte[] bytes;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "tx_id", nullable = false,
      foreignKey = @ForeignKey(name = "tx_metadata_tx_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Tx tx;

}
