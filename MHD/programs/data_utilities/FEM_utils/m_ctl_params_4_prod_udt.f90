!
!      module m_ctl_params_4_prod_udt
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine set_ctl_params_prod_udt                              &
!!     &         (mesh_file, udt_org_param, ucd)
!
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
!  ---------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_prod_udt                                &
     &         (mesh_file, udt_org_param, ucd)
!
      use calypso_mpi
      use t_file_IO_parameter
      use t_ucd_data
      use m_error_IDs
      use m_ctl_data_product_udt
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_data
      use ucd_IO_select
!
      type(field_IO_params), intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout)  :: udt_org_param
      type(ucd_data), intent(inout) :: ucd
!
!
      if (nprocs .ne. pu_plt%ndomain_ctl%intvalue) then
        write(e_message,*) 'Number of processes should be num. of mesh'
        call  calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
      call set_control_smp_def(my_rank, pu_plt)
      call set_control_mesh_def(pu_plt, mesh_file)
      call set_ucd_file_define(pu_plt, ucd)
      call set_control_mesh_file_def                                    &
     &   (def_org_ucd_header, org_pu_plt, udt_org_param)
!
!   set fiale name
!
      prod_udt_file1_head = "field/out"
      if (product_udt_1_head_ctl%iflag .ne. 0) then
        prod_udt_file1_head = product_udt_1_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'prod_udt_file1_head: ', trim(prod_udt_file1_head)
      end if
!
      prod_udt_file2_head = "field/out"
      if (product_udt_2_head_ctl%iflag .ne. 0) then
        prod_udt_file2_head = product_udt_2_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'prod_udt_file2_head: ', trim(prod_udt_file2_head)
      end if
!
      if (pu_plt%field_file_prefix%iflag .ne. 0) then
        result_udt_file_head = pu_plt%field_file_prefix%charavalue
      else
        result_udt_file_head = "field_new/out"
      end if
!
      call choose_ucd_file_format(pu_plt%field_file_fmt_ctl%charavalue, &
     &    pu_plt%field_file_fmt_ctl%iflag, ifmt_result_udt_file)
!
!
      product_field_1_name = "velocity"
      if (product_field_1_ctl%iflag .ne. 0) then
        product_field_1_name = product_field_1_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'product_field_1_name ', trim(product_field_1_name)
      end if
!
      product_field_2_name = "magnetic_field"
      if (product_field_2_ctl%iflag .ne. 0) then
        product_field_2_name = product_field_2_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'product_field_2_name ', trim(product_field_2_name)
      end if
!
      result_field_name =  "velocity"
      if (result_field_ctl%iflag .ne. 0) then
        result_field_name = result_field_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &   write(*,*) 'result_field_name ', trim(result_field_name)
      end if
!
      product_type = "Cartesian"
      if (product_type_ctl%iflag .ne. 0) then
        product_type = product_type_ctl%charavalue
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
