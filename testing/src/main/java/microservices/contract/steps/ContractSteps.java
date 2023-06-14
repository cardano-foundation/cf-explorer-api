package microservices.contract.steps;

import constants.Endpoints;
import core.BaseApi;
import io.qameta.allure.Step;
import microservices.common.constants.RequestParams;

import microservices.contract.models.Contract;
import microservices.contract.models.DataContract;
import org.testng.Assert;
import util.SortListUtil;

import java.util.ArrayList;
import java.util.Map;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class ContractSteps extends BaseApi {
    private Contract contract = new Contract();
    @Step("get list contract")
    public ContractSteps getListContracts(Map<String, Object> paramsContract){
        sendGet(Endpoints.ContractApi.GET_LIST_CONTRACT, paramsContract);
        return this;
    }
    @Step("verify number page of response get list contract")
    public ContractSteps verifyNumberPage(int pageNumberActual, Object pageNumberExpect){
        if(pageNumberExpect!=null){
            Assert.assertEquals(pageNumberActual, pageNumberExpect);
        }else{
            Assert.assertEquals(pageNumberActual, 0);
        }
        return this;
    }

    @Step("verify number size of response get list token")
    public ContractSteps verifySizeOfResponse(int sizeActual, Object sizeExpect){
        if(sizeExpect==null ){
            Assert.assertEquals(sizeActual, 20);
        }else {
            Assert.assertEquals(sizeActual, sizeExpect);
        }
        return this;
    }

    @Step("verify response data not null")
    public ContractSteps verifyResponseDataNotNull(ArrayList<DataContract> dataContracts){
        for (DataContract data:dataContracts)
             {
                 Assert.assertNotNull(data.getAddress());
                 Assert.assertNotNull(data.getTxCount());
                 Assert.assertNotNull(data.getBalance());
             }
        return this;
    }

    @Step("Verify contract response")
    public ContractSteps then_verifyContractResponse(Contract contract, Map<String, Object> params) {
        RequestParams requestParams = new RequestParams(params, 0, 20);
        assertThat(contract.getCurrentPage())
                .as("Value of field 'currentPage' is wrong")
                .isEqualTo(requestParams.getPage());
        assertThat(contract.getData().size())
                .as("The size of page is wrong")
                .isEqualTo(requestParams.getSize());
        if (requestParams.getSort()!=null) {
            boolean sorted = SortListUtil.isSortedByField(new ArrayList<>(contract.getData()), requestParams.getSort());
            assertThat(sorted).as("Contract is not sorted by inputted params").isEqualTo(true);
        }
        return this;
    }
}
