package com.cardano.explorer.view;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Embeddable;
import lombok.Getter;
import lombok.Setter;

@Embeddable
@Getter
@Setter
public class AddressTokenInputId implements Serializable {

  @Column(name = "address")
  private String address;

  @Column(name = "fingerprint")
  private String fingerprint;
}
