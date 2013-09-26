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
!
      implicit none
!
!
      character(len = kchara) :: org_mesh_head =   "mesh_fine/in"
      character(len = kchara) :: dest_mesh_head =  "mesh_coase/in"
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
      integer(kind = kint) :: ifmt_org_mesh_file = 0
      integer(kind = kint) :: ifmt_itp_mesh_file = 0
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
      use m_parallel_var_dof
      use m_machine_parameter
      use m_2nd_geometry_param
      use m_2nd_pallalel_vector
      use m_read_mesh_data
      use m_ctl_data_gen_table
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_sphere_bin_4_table
      use m_file_format_switch
      use set_control_platform_data
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
!
      if (i_mesh_header .ne. 0) then
        org_mesh_head = mesh_file_prefix
      end if
!
      if (i_new_mesh_head .ne. 0) then
        dest_mesh_head = new_mesh_prefix
      end if
!
      if (i_table_head_ctl .ne. 0) then
        table_file_head = table_head_ctl
      end if
!
      call choose_file_format(mesh_file_fmt_ctl, i_mesh_file_fmt,       &
     &    ifmt_org_mesh_file)
      call choose_file_format(new_mesh_file_fmt_ctl,                    &
     &    i_new_mesh_file_fmt, ifmt_itp_mesh_file)
!
      if (iflag_debug.eq.1)  then
        write(*,*) 'np_smp', np_smp, np_smp
        write(*,*) 'org_mesh_head: ',   trim(org_mesh_head)
        write(*,*) 'dest_mesh_head: ',  trim(dest_mesh_head)
        write(*,*) 'table_file_head: ', trim(table_file_head)
      end if
!
!
      if (i_num_subdomain .gt. 0) then
        ndomain_org = num_subdomain_ctl
      else
        ndomain_org = 1
      end if
      nprocs_2nd = ndomain_org
      if (iflag_debug.eq.1)   write(*,*) 'ndomain_org', nprocs_2nd
!
      if (i_num_new_domain .gt. 0) then
        ndomain_dest = num_new_domain_ctl
      else
        ndomain_dest = 1
      end if
!
      if (nprocs .ne. ndomain_dest) then
        write(e_message,*) 'Number of destination domains  ',           &
     &                   'shuld be the number of processes'
        call  parallel_abort(4000, e_message)
      end if
!
!
      if (i_reverse_ele_tbl .ne. 0) then
        if(     reverse_element_table_ctl .eq.'on'                      &
     &     .or. reverse_element_table_ctl .eq.'On'                      &
     &     .or. reverse_element_table_ctl .eq.'ON') then
          iflag_reverse_itp_tbl = 1
        end if
      end if
!
!
      if      ( ele_hash_type_ctl .eq. 'sphere'                         &
     &  .or.    ele_hash_type_ctl .eq. 'Sphere'                         &
     &  .or.    ele_hash_type_ctl .eq. 'SPHERE' ) then
        id_ele_hash_type = 1
      end if
!
      if (iflag_debug.eq.1)                                             &
     &   write(*,*) 'id_ele_hash_type', id_ele_hash_type
!
      if (id_ele_hash_type .eq. 1) then
!
        num_sph_grid(1) = num_r_divide_ctl
        num_sph_grid(2) = num_theta_divide_ctl
        num_sph_grid(3) = num_phi_divide_ctl
!
        num_sph_bin(1) = num_sph_grid(1) + 1
        num_sph_bin(2) = num_sph_grid(2)
        num_sph_bin(3) = num_sph_grid(3)
!
        call allocate_sphere_divide_points
!
        if (num_sph_grid(1) .gt. 0) then
          r_divide(1:num_sph_grid(1)) = r_divide_ctl(1:num_sph_grid(1))
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'num_sph_grid',  num_sph_grid
          write(*,*) 'num_sph_bin', num_sph_bin
          write(*,*) 'r_divide', r_divide
        end if
!
      end if
!
!
!
      maxitr =   itr_refine_ctl
      eps_iter = eps_refine_ctl
!
      if (iflag_debug.eq.1)  then
        write(*,*) 'maxitr ', maxitr
        write(*,*) 'eps_iter ', eps_iter
      end if
!
      num_search_times = num_search_times_ctl
      call allocate_search_param
!
      if (num_search_times .gt. 0) then
        i_search_sleeve(1:num_search_times)                             &
     &      = i_search_sleeve_ctl(1:num_search_times)
        search_error_level(1:num_search_times)                          &
     &      = search_error_level_ctl(1:num_search_times)
!
        call deallocate_search_param_ctl
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
