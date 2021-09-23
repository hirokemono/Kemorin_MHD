!>@file   set_ctl_interpolation.f90
!!@brief  module set_ctl_interpolation
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  SEt control paramteres for interpolation
!!
!!@verbatim
!!      subroutine set_ctl_params_interpolation                         &
!!     &         (gtbl_ctl, gen_itp_p, nprocs_2nd)
!!        type(ctl_data_gen_table), intent(in) :: gtbl_ctl
!!        type(ctl_params_4_gen_table), intent(inout) :: gen_itp_p
!!@endverbatim
!
      module set_ctl_interpolation
!
      use m_precision
!
      use calypso_mpi
      use m_error_IDs
!
      use t_file_IO_parameter
      use t_IO_step_parameter
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &             :: table_file_head = "mesh/table"
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_interpolation                           &
     &         (gtbl_ctl, gen_itp_p, nprocs_2nd)
!
      use m_machine_parameter
      use m_file_format_switch
      use m_field_file_format
      use t_ctl_params_4_gen_table
      use t_ctl_data_gen_table
      use itp_table_file_IO_select
      use set_control_platform_item
      use set_control_platform_data
      use parallel_ucd_IO_select
!
      type(ctl_data_gen_table), intent(in) :: gtbl_ctl
      type(ctl_params_4_gen_table), intent(inout) :: gen_itp_p
      integer, intent(inout) :: nprocs_2nd
!
      integer(kind = kint) :: nmax
!
      call turn_off_debug_flag_by_ctl(my_rank, gtbl_ctl%src_plt)
      call set_control_smp_def(my_rank, gtbl_ctl%src_plt)
!
      call set_control_parallel_mesh(gtbl_ctl%src_plt,                  &
     &                               gen_itp_p%itp_org_mesh_file)
!
      if (gtbl_ctl%dst_plt%mesh_file_prefix%iflag .ne. 0) then
        gen_itp_p%itp_dest_mesh_file%file_prefix                        &
     &     = gtbl_ctl%dst_plt%mesh_file_prefix%charavalue
      end if
!
      if (gtbl_ctl%table_head_ctl%iflag .ne. 0) then
        gen_itp_p%itp_file_IO%file_prefix                               &
     &         = gtbl_ctl%table_head_ctl%charavalue
      else
        gen_itp_p%itp_file_IO%file_prefix = table_file_head
      end if
!
      if (gtbl_ctl%single_itp_tbl_head_ctl%iflag .ne. 0) then
        gen_itp_p%sgl_table_file_head                                   &
     &     = gtbl_ctl%single_itp_tbl_head_ctl%charavalue
      end if
!
      if (iflag_debug.eq.1) then
        write(*,*) 'np_smp', np_smp, np_smp
        write(*,*) 'org_mesh_head: ',                                   &
     &            trim(gen_itp_p%itp_org_mesh_file%file_prefix)
        write(*,*) 'dest_mesh_head: ',                                  &
     &            trim(gen_itp_p%itp_dest_mesh_file%file_prefix)
        write(*,*) 'table_file_head: ',                                 &
     &            trim(gen_itp_p%itp_file_IO%file_prefix)
      end if
!
      if (gtbl_ctl%itp_node_head_ctl%iflag .ne. 0) then
        itp_node_file_head = gtbl_ctl%itp_node_head_ctl%charavalue
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'itp_node_file_head: ', trim(itp_node_file_head)
      end if
!
!
      call set_ctl_parallel_file_w_def(def_org_rst_prefix,              &
     &    gtbl_ctl%src_plt%restart_file_prefix,                         &
     &    gtbl_ctl%src_plt%restart_file_fmt_ctl, gen_itp_p%org_fst_IO)
!
      call set_ctl_parallel_file_w_def(def_itp_rst_prefix,              &
     &    gtbl_ctl%dst_plt%restart_file_prefix,                         &
     &    gtbl_ctl%dst_plt%restart_file_fmt_ctl, gen_itp_p%itp_fst_IO)
!
      call set_merged_ucd_file_define                                   &
     &   (gtbl_ctl%src_plt, gen_itp_p%org_ucd_IO)
      call set_merged_ucd_file_ctl(itp_udt_file_head,                   &
     &    gtbl_ctl%dst_plt%field_file_prefix,                           &
     &    gtbl_ctl%dst_plt%field_file_fmt_ctl, gen_itp_p%itp_ucd_IO)
!
      call set_interpolate_domains_ctl(gtbl_ctl, gen_itp_p,             &
     &                                 nprocs_2nd)
!
      gen_itp_p%itp_dest_mesh_file%iflag_format                         &
     &   = choose_file_format(gtbl_ctl%dst_plt%mesh_file_fmt_ctl)
!
      gen_itp_p%itp_file_IO%iflag_format                                &
     &   = choose_file_format(gtbl_ctl%fmt_itp_table_file_ctl)
!
      nmax = max(gen_itp_p%ndomain_org,gen_itp_p%ndomain_dest)
      if(nprocs .ne. nmax) then
        write(e_message,*)                                              &
     &     'Num. of rank is larger num. of orgin or destinate dom.'
        call  calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
      end subroutine set_ctl_params_interpolation
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_interpolation
