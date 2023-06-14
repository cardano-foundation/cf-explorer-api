package microservices.contract.models;

import lombok.Data;

import java.util.ArrayList;

@Data
public class Contract {
    private ArrayList<DataContract> data ;
    private int totalItems;
    private int totalPages;
    private int currentPage;
}
