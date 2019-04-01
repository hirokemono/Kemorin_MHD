!
!      module t_ctl_params_4_gen_table
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine alloc_search_param(gen_itp_p)
!!      subroutine dealloc_search_param(gen_itp_p)
!!      subroutine set_ctl_params_4_gen_table                           &
!!     &         (gtbl_ctl, gen_itp_p, itp_blks)
!!      subroutine set_interpolate_domains_ctl(gtbl_ctl, gen_itp_p)
!!        type(ctl_data_gen_table), intent(in) :: gtbl_ctl
!
      module t_ctl_params_4_gen_table
!
      use m_precision
      use m_field_file_format
      use t_file_IO_parameter
!
      implicit none
!
!
      character(len = kchara), parameter                                &
     &             :: def_org_rst_prefix = "restart/rst"
!
      character(len = kchara) :: itp_node_file_head = "node_test_itp"
      character(len = kchara), parameter                                &
     &             :: def_itp_rst_prefix = "rst_new/rst"
      character(len = kchara), parameter                                &
     &             :: itp_udt_file_head = "field_new/out"
!
      type ctl_params_4_gen_table
        type(field_IO_params) :: itp_org_mesh_file
        type(field_IO_params) :: itp_dest_mesh_file
!
        type(field_IO_params) :: org_fst_IO
        type(field_IO_params) :: itp_fst_IO
!
        type(field_IO_params) :: org_ucd_IO
        type(field_IO_params) :: itp_ucd_IO
!
        character(len = kchara) :: table_file_head = "mesh/table"
        character(len = kchara)                                         &
     &             :: sgl_table_file_head = "single_itp_table"
!
        integer(kind = kint) :: iflag_reverse_itp_tbl = 0
!
        integer :: ndomain_org = 1
        integer :: ndomain_dest = 1
!
        character(len = kchara) :: ele_hash_type = "sphere"
        integer(kind = kint) :: id_ele_hash_type = 1
!
        integer(kind = kint) :: num_r_divide =     1
        integer (kind=kint) :: maxitr = 20000
        real (kind=kreal) :: eps_iter = 1.0d-15
!
        integer(kind = kint) :: num_search_times = 0
        integer(kind = kint), allocatable :: i_search_sleeve(:)
        real(kind = kreal), allocatable :: search_error_level(:)
      end type ctl_params_4_gen_table
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_search_param(gen_itp_p)
!
      type(ctl_params_4_gen_table), intent(inout) :: gen_itp_p
!
      integer(kind = kint) :: num
!
      num = gen_itp_p%num_search_times
      allocate(gen_itp_p%i_search_sleeve(0:num))
      allocate(gen_itp_p%search_error_level(0:num) )
      gen_itp_p%i_search_sleeve = 0
      gen_itp_p%search_error_level = 0.0d0
!
      end subroutine alloc_search_param
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_search_param(gen_itp_p)
!
      type(ctl_params_4_gen_table), intent(inout) ::  gen_itp_p
!
      deallocate(gen_itp_p%i_search_sleeve)
      deallocate(gen_itp_p%search_error_level)
!
      end subroutine dealloc_search_param
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_4_gen_table                             &
     &         (gtbl_ctl, gen_itp_p, itp_blks)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_2nd_pallalel_vector
      use m_file_format_switch
      use m_default_file_prefix
      use t_ctl_data_gen_table
      use t_search_block_4_itp
      use itp_table_IO_select_4_zlib
      use set_control_platform_data
      use skip_comment_f
!
      type(ctl_data_gen_table), intent(in) :: gtbl_ctl
!
      type(ctl_params_4_gen_table), intent(inout) :: gen_itp_p
      type(para_block_4_interpolate), intent(inout) :: itp_blks
!
!
      call turn_off_debug_flag_by_ctl(my_rank, gtbl_ctl%src_plt)
      call set_control_smp_def(my_rank, gtbl_ctl%src_plt)
!
      call set_control_mesh_def                                         &
     &   (gtbl_ctl%src_plt, gen_itp_p%itp_org_mesh_file)
!
      if (gtbl_ctl%table_head_ctl%iflag .ne. 0) then
        gen_itp_p%table_file_head = gtbl_ctl%table_head_ctl%charavalue
      end if
!
      call set_control_mesh_file_def(def_new_mesh_head,                 &
     &    gtbl_ctl%dst_plt, gen_itp_p%itp_dest_mesh_file)
      ifmt_itp_table_file                                               &
     &    = choose_file_format(gtbl_ctl%fmt_itp_table_file_ctl)
!
      if (iflag_debug.eq.1)  then
        write(*,*) 'np_smp', np_smp, np_smp
        write(*,*) 'org_mesh_head: ',                                   &
     &            trim(gen_itp_p%itp_org_mesh_file%file_prefix)
        write(*,*) 'dest_mesh_head: ',                                  &
     &            trim(gen_itp_p%itp_dest_mesh_file%file_prefix)
        write(*,*) 'table_file_head: ', trim(gen_itp_p%table_file_head)
      end if
!
!
      call set_interpolate_domains_ctl(gtbl_ctl, gen_itp_p)
!
      if (nprocs .ne. gen_itp_p%ndomain_dest) then
        write(e_message,*) 'Number of destination domains  ',           &
     &                   'shuld be the number of processes'
        call  calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
!
      if (gtbl_ctl%reverse_element_table_ctl%iflag .ne. 0               &
     &   .and. yes_flag(gtbl_ctl%reverse_element_table_ctl%charavalue)  &
     & ) then
        gen_itp_p%iflag_reverse_itp_tbl = 1
      end if
!
!
      if (iflag_debug.eq.1)                                             &
     &   write(*,*) 'id_ele_hash_type', gen_itp_p%id_ele_hash_type
!
      itp_blks%num_xyz_block(1:3) = 1
      if (gen_itp_p%id_ele_hash_type .eq. 1) then
        if(gtbl_ctl%num_radial_divide_ctl%iflag .gt. 0) then
          itp_blks%num_xyz_block(1)                                     &
     &         = gtbl_ctl%num_radial_divide_ctl%intvalue
        end if
        if(gtbl_ctl%num_theta_divide_ctl%iflag .gt. 0) then
          itp_blks%num_xyz_block(2)                                     &
     &         = gtbl_ctl%num_theta_divide_ctl%intvalue
        end if
        if(gtbl_ctl%num_phi_divide_ctl%iflag .gt. 0) then
          itp_blks%num_xyz_block(3)                                     &
     &         = gtbl_ctl%num_phi_divide_ctl%intvalue
        end if
        if (iflag_debug.eq.1) then
          write(*,*) 'num_xyz_block',  itp_blks%num_xyz_block
        end if
      else
        if(gtbl_ctl%num_x_divide_ctl%iflag .gt. 0) then
          itp_blks%num_xyz_block(1)                                     &
     &         = gtbl_ctl%num_x_divide_ctl%intvalue
        end if
        if(gtbl_ctl%num_y_divide_ctl%iflag .gt. 0) then
          itp_blks%num_xyz_block(2)                                     &
     &         = gtbl_ctl%num_y_divide_ctl%intvalue
        end if
        if(gtbl_ctl%num_z_divide_ctl%iflag .gt. 0) then
          itp_blks%num_xyz_block(3)                                     &
     &         = gtbl_ctl%num_z_divide_ctl%intvalue
        end if
      end if
!
      if (iflag_debug.eq.1) then
        write(*,*) 'id_ele_hash_type',  gen_itp_p%id_ele_hash_type
        write(*,*) 'num_xyz_block',  itp_blks%num_xyz_block
      end if
!
!
      if(gtbl_ctl%itr_refine_ctl%iflag .gt. 0) then
        gen_itp_p%maxitr = gtbl_ctl%itr_refine_ctl%intvalue
      end if
      if(gtbl_ctl%eps_refine_ctl%iflag .gt. 0) then
        gen_itp_p%eps_iter = gtbl_ctl%eps_refine_ctl%realvalue
      end if
!
      if (iflag_debug.eq.1)  then
        write(*,*) 'maxitr ', gen_itp_p%maxitr
        write(*,*) 'eps_iter ', gen_itp_p%eps_iter
      end if
!
      gen_itp_p%num_search_times = gtbl_ctl%eps_4_itp_ctl%num
      call alloc_search_param(gen_itp_p)
!
      if (gen_itp_p%num_search_times .gt. 0) then
        gen_itp_p%i_search_sleeve(1:gen_itp_p%num_search_times)         &
     &      = gtbl_ctl%eps_4_itp_ctl%ivec(1:gen_itp_p%num_search_times)
        gen_itp_p%search_error_level(1:gen_itp_p%num_search_times)      &
     &      = gtbl_ctl%eps_4_itp_ctl%vect(1:gen_itp_p%num_search_times)
!
      end if
!
      if (iflag_debug.eq.1)  then
        write(*,*) 'num_search_times ', gen_itp_p%num_search_times
        write(*,*) 'i_search_sleeve ', gen_itp_p%i_search_sleeve
        write(*,*) 'search_error_level ', gen_itp_p%search_error_level
      end if
!
      end subroutine set_ctl_params_4_gen_table
!
!  ---------------------------------------------------------------------
!
      subroutine set_interpolate_domains_ctl(gtbl_ctl, gen_itp_p)
!
      use t_ctl_data_gen_table
      use m_2nd_pallalel_vector
!
      type(ctl_data_gen_table), intent(in) :: gtbl_ctl
      type(ctl_params_4_gen_table), intent(inout) :: gen_itp_p
!
!
      gen_itp_p%ndomain_org = 1
      if (gtbl_ctl%src_plt%ndomain_ctl%iflag .gt. 0) then
        gen_itp_p%ndomain_org                                           &
     &       = int(gtbl_ctl%src_plt%ndomain_ctl%intvalue)
      end if
!
      nprocs_2nd = gen_itp_p%ndomain_org
      if (iflag_debug.eq.1) write(*,*) 'ndomain_org', nprocs_2nd
!
      if (gtbl_ctl%dst_plt%ndomain_ctl%iflag .gt. 0) then
        gen_itp_p%ndomain_dest                                          &
     &        = int(gtbl_ctl%dst_plt%ndomain_ctl%intvalue)
      else
        gen_itp_p%ndomain_dest = 1
      end if
!
      end subroutine set_interpolate_domains_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_params_4_gen_table
