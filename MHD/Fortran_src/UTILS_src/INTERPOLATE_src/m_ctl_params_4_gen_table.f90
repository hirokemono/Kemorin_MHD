!
!      module m_ctl_params_4_gen_table
!
!     Written by H. Matsui on July, 2006
!
!      subroutine allocate_search_param
!      subroutine deallocate_search_param
!      subroutine set_ctl_params_4_gen_table
!
      module m_ctl_params_4_gen_table
!
      use m_precision
      use m_field_file_format
      use t_file_IO_parameter
!
      implicit none
!
!
      type(field_IO_params), save :: itp_org_mesh_file
!
      type(field_IO_params), save :: itp_dest_mesh_file
!
      character(len = kchara) :: table_file_head = "mesh/table"
      character(len = kchara)                                           &
     &             :: sgl_table_file_head = "single_itp_table"
!
      character(len = kchara) :: org_rst_file_head = "restart/rst"
      character(len = kchara) :: org_udt_file_head = "field/out"
!
      character(len = kchara) :: itp_node_file_head = "node_test_itp"
      character(len = kchara) :: itp_rst_file_head = "rst_new/rst"
      character(len = kchara) :: itp_udt_file_head = "field_new/out"
!
      integer(kind = kint) :: ifmt_org_rst_file =  0
      integer(kind = kint) :: ifmt_itp_rst_file =  0
!
      integer(kind = kint) :: itype_org_udt_file =  iflag_fld
      integer(kind = kint) :: itype_itp_udt_file =  iflag_fld
!
      integer(kind = kint) :: iflag_reverse_itp_tbl = 0
!
      integer(kind = kint) :: ndomain_org = 1
      integer(kind = kint) :: ndomain_dest = 1
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
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_search_param
!
      allocate(i_search_sleeve(0:num_search_times) )
      allocate(search_error_level(0:num_search_times) )
      i_search_sleeve = 0
      search_error_level = 0.0d0
!
      end subroutine allocate_search_param
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_search_param
!
      deallocate(i_search_sleeve)
      deallocate(search_error_level)
!
      end subroutine deallocate_search_param
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_4_gen_table
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_2nd_pallalel_vector
      use m_ctl_data_gen_table
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_search_bolck_4_itp
      use m_file_format_switch
      use m_default_file_prefix
      use itp_table_IO_select_4_zlib
      use set_control_platform_data
      use skip_comment_f
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt1)
      call set_control_smp_def(my_rank, plt1)
!
      call set_control_mesh_def(plt1, itp_org_mesh_file)
!
      if (table_head_ctl%iflag .ne. 0) then
        table_file_head = table_head_ctl%charavalue
      end if
!
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, new_plt, itp_dest_mesh_file)
      call choose_file_format                                           &
     &   (fmt_itp_table_file_ctl, ifmt_itp_table_file)
!
      if (iflag_debug.eq.1)  then
        write(*,*) 'np_smp', np_smp, np_smp
        write(*,*) 'org_mesh_head: ',                                   &
     &            trim(itp_org_mesh_file%file_prefix)
        write(*,*) 'dest_mesh_head: ',                                  &
     &            trim(itp_dest_mesh_file%file_prefix)
        write(*,*) 'table_file_head: ', trim(table_file_head)
      end if
!
!
      ndomain_org = 1
      if (plt1%ndomain_ctl%iflag .gt. 0) then
        ndomain_org = plt1%ndomain_ctl%intvalue
      end if
!
      nprocs_2nd = ndomain_org
      if (iflag_debug.eq.1)   write(*,*) 'ndomain_org', nprocs_2nd
!
      if (new_plt%ndomain_ctl%iflag .gt. 0) then
        ndomain_dest = new_plt%ndomain_ctl%intvalue
      else
        ndomain_dest = 1
      end if
!
      if (nprocs .ne. ndomain_dest) then
        write(e_message,*) 'Number of destination domains  ',           &
     &                   'shuld be the number of processes'
        call  calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
!
      if (reverse_element_table_ctl%iflag .ne. 0                        &
     &    .and. yes_flag(reverse_element_table_ctl%charavalue)) then
        iflag_reverse_itp_tbl = 1
      end if
!
!
      if (iflag_debug.eq.1)                                             &
     &   write(*,*) 'id_ele_hash_type', id_ele_hash_type
!
      num_xyz_block(1:3) = 1
      if (id_ele_hash_type .eq. 1) then
        if(num_radial_divide_ctl%iflag .gt. 0) then
          num_xyz_block(1) = num_radial_divide_ctl%intvalue
        end if
        if(num_theta_divide_ctl%iflag .gt. 0) then
          num_xyz_block(2) = num_theta_divide_ctl%intvalue
        end if
        if(num_phi_divide_ctl%iflag .gt. 0) then
          num_xyz_block(3) = num_phi_divide_ctl%intvalue
        end if
        if (iflag_debug.eq.1) then
          write(*,*) 'num_xyz_block',  num_xyz_block
        end if
      else
        if(num_x_divide_ctl%iflag .gt. 0) then
          num_xyz_block(1) = num_x_divide_ctl%intvalue
        end if
        if(num_y_divide_ctl%iflag .gt. 0) then
          num_xyz_block(2) = num_y_divide_ctl%intvalue
        end if
        if(num_z_divide_ctl%iflag .gt. 0) then
          num_xyz_block(3) = num_z_divide_ctl%intvalue
        end if
      end if
!
      if (iflag_debug.eq.1) then
        write(*,*) 'id_ele_hash_type',  id_ele_hash_type
        write(*,*) 'num_xyz_block',  num_xyz_block
      end if
!
!
      if(itr_refine_ctl%iflag .gt. 0) maxitr = itr_refine_ctl%intvalue
      if(eps_refine_ctl%iflag .gt. 0) then
        eps_iter = eps_refine_ctl%realvalue
      end if
!
      if (iflag_debug.eq.1)  then
        write(*,*) 'maxitr ', maxitr
        write(*,*) 'eps_iter ', eps_iter
      end if
!
      num_search_times = eps_4_itp_ctl%num
      call allocate_search_param
!
      if (num_search_times .gt. 0) then
        i_search_sleeve(1:num_search_times)                             &
     &      = eps_4_itp_ctl%ivec(1:num_search_times)
        search_error_level(1:num_search_times)                          &
     &      = eps_4_itp_ctl%vect(1:num_search_times)
!
        call dealloc_control_array_i_r(eps_4_itp_ctl)
      end if
!
!
      if (iflag_debug.eq.1)  then
        write(*,*) 'num_search_times ', num_search_times
        write(*,*) 'i_search_sleeve ', i_search_sleeve
        write(*,*) 'search_error_level ', search_error_level
      end if
!
      end subroutine set_ctl_params_4_gen_table
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_params_4_gen_table
