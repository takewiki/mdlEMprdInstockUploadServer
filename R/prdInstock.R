#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdInstockUploadServer()
prdInstockUploadServer <- function(input, output, session, dms_token, erp_token) {

  files_prdInstock = tsui::var_file('files_prdInstock')

  shiny::observeEvent(input$btn_prdInstock_upload, {

    filename=files_prdInstock()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      mdlEMprdInstockUploadPkg::dms_prd_instock_input_delete(dms_token =dms_token )

      mdlEMprdInstockUploadPkg::dms_prd_instockEntry_input_delete(dms_token =dms_token )



      data <- readxl::read_excel(filename,col_types = c("text", "text", "text",
                                                        "date", "text", "text", "numeric",
                                                        "text", "text", "numeric", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "numeric", "text",
                                                        "text", "text"))

      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      tsda::db_writeTable2(token = erp_token,table_name = 'rds_src_ods_t_prd_instock_input',r_object = data,append = TRUE)

      mdlEMprdInstockUploadPkg::erp_prdInstock_input_update(erp_token =erp_token )


      data_erp = mdlEMprdInstockUploadPkg::erp_prdInstock_select(erp_token =erp_token )

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_ods_t_prd_instock_input',r_object = data_erp,append = TRUE)


      dataEntry_erp = mdlEMprdInstockUploadPkg::erp_prdInstockEntry_select(erp_token =erp_token )


      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_ods_t_prd_instockEntry_input',r_object = dataEntry_erp,append = TRUE)



      mdlEMprdInstockUploadPkg::dms_prd_instock_upload(dms_token =dms_token )


      mdlEMprdInstockUploadPkg::dms_prd_instockEntry_upload(dms_token =dms_token )



      tsui::pop_notice("上传成功")


    }

  })
}







#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdInstockViewServer()
prdInstockViewServer <- function(input, output, session, dms_token, erp_token) {



  shiny::observeEvent(input$btn_prdInstock_view, {



    text_prdInstock_FBillNo = tsui::var_text("text_prdInstock_FBillNo")

    text_date_prdInstock_FDate = tsui::var_dateRange('text_date_prdInstock_FDate')


    FDate = text_date_prdInstock_FDate()

    FStartDate = FDate[1]

    FEndDate = FDate[2]

    FBillNo=text_prdInstock_FBillNo()



    data = mdlEMprdInstockUploadPkg::dms_prd_instock_view(dms_token = dms_token,FBillNo =FBillNo,FStartDate,FEndDate)

    tsui::run_dataTable2(id = 'prdInstock_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_prdInstock',data = data,filename='生产入库上传日志查询.xlsx')



  })






}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdInstockSyncServer()
prdInstockSyncServer <- function(input, output, session, dms_token, erp_token) {



  shiny::observeEvent(input$btn_prdInstock_sync, {


    mdlEMprdInstockUploadr::prdInstock_ERPSync()

    mdlEMprdInstockUploadPkg::erp_prdInstock_orderno_update(erp_token = erp_token)

    tsui::pop_notice("回传成功")



  })






}




#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdInstockServer()
prdInstockServer <- function(input, output, session, dms_token, erp_token) {
  prdInstockUploadServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)


  prdInstockViewServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)

  prdInstockSyncServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)
}
