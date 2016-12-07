!
!      module m_ctl_params_4_prod_udt
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine set_ctl_params_prod_udt(udt_org_param, ucd)

      module m_ctl_params_4_prod_udt
!
      use m_precision
!
      implicit none
!
!
      character(len = kchara) :: prod_udt_file1_head = "field_1/out"
      character(len = kchara) :: prod_udt_file2_head = "field_2/out"
!
      character(len = kchara) :: result_udt_file_head= "field_prod/out"
      integer(kind = kint) :: ifmt_result_udt_file = 0
!
      character(len = kchara) :: product_field_1_name = "velocity"
      integer(kind = kint) :: i_field_product1 = 1
      integer(kind = kint) :: ncomp_4_product1 = 3
      character(len = kchara) :: product_field_2_name = "velocity"
      integer(kind = kint) :: i_field_product2 = 1
      integer(kind = kint) :: ncomp_4_product2 = 3
!
      character(len = kchara) :: product_type =  "dot"
      integer(kind = kint) :: iflag_product_type = 0
!
      character(len = kchara) :: result_field_name = "velocity"
      integer(kind = kint) :: i_field_4_result = 1
      integer(kind = kint) :: ncomp_4_result =   3
!
!
      integer(kind = kint) :: i_diff_steps = 0
      real(kind = kreal) :: dt =  1.0d0
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_prod_udt(udt_org_param, ucd)
!
      use calypso_mpi
      use t_file_IO_parameter
      use t_ucd_data
      use m_error_IDs
      use m_read_mesh_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_ctl_data_product_udt
      use m_file_format_switch
      use set_control_platform_data
      use set_ctl_params_2nd_files
      use ucd_IO_select
!
      type(field_IO_params), intent(inout)  :: udt_org_param
      type(ucd_data), intent(inout) :: ucd
!
!
      if (nprocs .ne. ndomain_ctl%intvalue) then
        write(e_message,*) 'Number of processes should be num. of mesh'
        call  calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
      call set_control_smp_def(my_rank)
      call set_control_mesh_def(mesh1_file)
      call set_control_org_udt_file_def(udt_org_param)
      call set_ucd_file_define(ucd)
!
!   set fiale name
!
      if (i_product_udt_1 .ne. 0) then
        prod_udt_file1_head = product_udt_1_head_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'prod_udt_file1_head: ', trim(prod_udt_file1_head)
      end if
!
      if (i_product_udt_2 .ne. 0) then
        prod_udt_file2_head = product_udt_2_head_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'prod_udt_file2_head: ', trim(prod_udt_file2_head)
      end if
!
      if (udt_file_head_ctl%iflag .ne. 0) then
        result_udt_file_head = udt_file_head_ctl%charavalue
      else
        result_udt_file_head = "field_new/out"
      end if
!
      call choose_ucd_file_format(udt_file_fmt_ctl%charavalue,          &
     &    udt_file_fmt_ctl%iflag, ifmt_result_udt_file)
!
!
      if (i_product_udt_1 .ne. 0) then
        product_field_1_name = product_field_1_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'product_field_1_name ', trim(product_field_1_name)
      end if
!
      if (i_product_udt_2 .ne. 0) then
        product_field_2_name = product_field_2_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'product_field_2_name ', trim(product_field_2_name)
      end if
!
      if (i_result_field .ne. 0) then
        result_field_name = result_field_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'result_field_name ', trim(result_field_name)
      end if
!
      if (i_product_type .ne. 0) then
        product_type = product_type_ctl
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'product_type ', trim(product_type)
      end if
!
      if(product_type.eq.'dot' .and. product_type.eq.'Dot'              &
     &   .and. product_type.eq.'DOT') then
        iflag_product_type = 1
      else if(product_type.eq.'cross' .and. product_type.eq.'Cross'     &
     &   .and. product_type.eq.'CROSS') then
        iflag_product_type = 2
      else if(product_type.eq.'matvec' .and. product_type.eq.'Matvec'   &
     &   .and. product_type.eq.'MATVEC') then
        iflag_product_type = 3
      else
        iflag_product_type = 0
      end if
!
      end subroutine set_ctl_params_prod_udt
!
!   --------------------------------------------------------------------
!
      end module m_ctl_params_4_prod_udt
