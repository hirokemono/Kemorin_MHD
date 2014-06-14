!trans_filter_moms_newdomain.f90
!      module trans_filter_moms_newdomain
!
!      modified by H. Matsui on Apr., 2008
!
!      subroutine trans_filter_moms_newmesh_para
!      subroutine trans_filter_moms_newmesh_sgl
!
      module trans_filter_moms_newdomain
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_ctl_param_newdom_filter
      use m_comm_data_IO
      use m_read_mesh_data
      use m_read_boundary_data
      use mesh_IO_select
      use set_parallel_file_name
!
      implicit none
!
      private :: count_nele_newdomain_para
      private :: count_nele_newdomain_single
      private :: trans_filter_moms_each_domain
      private :: const_filter_moms_newdomain
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_newmesh_para
!
      use calypso_mpi
!
      use set_filter_moms_2_new_mesh
!
!
      call count_nele_newdomain_para(my_rank)
      call allocate_iele_local_newfilter
!
      call trans_filter_moms_each_domain(my_rank)
!
      call deallocate_iele_local_newfilter
!
      end subroutine trans_filter_moms_newmesh_para
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_newmesh_sgl
!
      use m_2nd_pallalel_vector
      use set_filter_moms_2_new_mesh
!
      integer(kind = kint) :: ip2, my_rank_2nd
!
!
      call count_nele_newdomain_single
!
      call allocate_iele_local_newfilter
!
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
        call trans_filter_moms_each_domain(my_rank_2nd)
      end do
!
      call deallocate_iele_local_newfilter
!
      end subroutine trans_filter_moms_newmesh_sgl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_nele_newdomain_para(my_rank_2nd)
!
      use set_filter_moms_2_new_mesh
!
      integer(kind = kint), intent(in) :: my_rank_2nd
      integer(kind = kint) :: iele
!
!
      mesh_file_head = target_mesh_head
      call sel_read_mesh_geometry(my_rank_2nd)
!
      mesh_file_head = org_mesh_head
!
      max_gl_ele_newdomain = globalelmid_dummy(1)
      do iele = 2, numele_dummy
        max_gl_ele_newdomain                                            &
     &         = max(max_gl_ele_newdomain,globalelmid_dummy(iele))
      end do
!
      call deallocate_mesh_arrays
      call deallocate_comm_item_IO
!
      end subroutine count_nele_newdomain_para
!
!   --------------------------------------------------------------------
!
      subroutine count_nele_newdomain_single
!
      use m_2nd_pallalel_vector
      use set_filter_moms_2_new_mesh
!
      integer(kind = kint) :: ip2, my_rank_2nd
!
!
      max_gl_ele_newdomain = 0
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
!
        mesh_file_head = target_mesh_head
        iflag_mesh_file_fmt = id_ascii_file_fmt
        call sel_read_geometry_size(my_rank_2nd)
        call deallocate_node_data_dummy
        call deallocate_neib_domain_IO
!
        max_gl_ele_newdomain = max_gl_ele_newdomain + numele_dummy
      end do
!
      end subroutine count_nele_newdomain_single
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_each_domain(my_rank_2nd)
!
      use calypso_mpi
      use m_2nd_geometry_data
      use m_2nd_filter_moments
      use m_2nd_filter_ele_length
      use m_filter_file_names
      use m_filter_moments
      use m_filter_elength
      use filter_moment_IO_select
      use set_filter_moms_2_new_mesh
      use set_2nd_ele_connect_4_IO
      use set_filter_moms_2_new_mesh
      use copy_filter_moms_from_2nd
!
      integer(kind = kint), intent(in) :: my_rank_2nd
!
!
      iflag_mesh_file_fmt = id_ascii_file_fmt
      mesh_file_head = target_mesh_head
      call sel_read_mesh(my_rank_2nd)
      mesh_file_head = org_mesh_head
!
      call deallocate_boundary_arrays
      call deallocate_node_data_dummy
      call deallocate_comm_item_IO
!
      node_2nd%numnod = numnod_dummy
      node_2nd%internal_node = internal_node_dummy
      call copy_2nd_ele_connect_from_IO
!
!    construct new filter table
!
      if (iflag_set_filter_elen .gt. 0) then
        call allocate_2nd_ele_length(ele_2nd%numele)
      end if
!
      if (iflag_set_filter_moms .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = org_filter_moms_head
        call sel_read_num_filter_mom_file(izero)
!
        num_filter_moms_2nd = num_filter_moms
        call allocate_2nd_filter_moms_ele(ele_2nd%numele, num_filter_moms)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'set_iele_table_4_newfilter'
      call set_iele_table_4_newfilter
!
      if (iflag_debug.eq.1) write(*,*) 'const_filter_moms_newdomain'
      call const_filter_moms_newdomain(nprocs)
!
!
!      write new filter moments file
!
      if (iflag_set_filter_moms .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = new_filter_moms_head
        call copy_filter_moments_from_2nd
        call sel_write_filter_moms_file(my_rank_2nd)
        call deallocate_filter_moms_ele
      end if
!
!
      if (iflag_set_filter_elen .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = new_filter_elen_head
        call copy_elength_ele_from_2nd
        call sel_write_filter_elen_file(my_rank_2nd)
        call deallocate_ele_length
      end if
!
      call deallocate_ref_1d_moment
      call deallocate_ele_connect_type(ele_2nd)
!
      end subroutine trans_filter_moms_each_domain
!
!   --------------------------------------------------------------------
!
      subroutine const_filter_moms_newdomain(norg_domain)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_filter_file_names
      use m_filter_moments
      use m_filter_elength
      use filter_moment_IO_select
      use set_element_connect_4_IO
      use set_filter_moms_2_new_mesh
!
      integer(kind = kint), intent(in) :: norg_domain
      integer(kind = kint) :: ip, my_rank_org, ierr
!
!
      do ip = 1, norg_domain
        my_rank_org = ip - 1
!
        iflag_mesh_file_fmt = id_ascii_file_fmt
        mesh_file_head = target_mesh_head
        call sel_read_mesh(my_rank_org)
!
        call deallocate_boundary_arrays
        call deallocate_node_data_dummy
        call deallocate_comm_item_IO
!
        call copy_element_connect_from_IO
!
!  read element length data
!
        if (iflag_set_filter_moms .gt. 0) then
          ifmt_filter_file = id_ascii_file_fmt
          filter_file_head = org_filter_moms_head
          call sel_read_filter_moms_file(my_rank_org,                   &
     &        numnod, numele, ierr)
!
          call set_new_filter_moms_ele
          call deallocate_filter_moms_ele
          if (ip .lt. norg_domain .or. iflag_set_filter_elen.gt.0) then
            call deallocate_ref_1d_moment
          end if
        end if
!
!
        if (iflag_set_filter_elen .gt. 0) then
          ifmt_filter_file = id_ascii_file_fmt
          filter_file_head = org_filter_elen_head
          call sel_read_filter_elen_file(my_rank_org,                   &
     &        numnod, numele, ierr)
!
!          if (iflag_debug.eq.1) write(*,*) 'set_new_elength_ele'
          call set_new_elength_ele
!          if (iflag_debug.eq.1) write(*,*) 'deallocate_ele_length'
          call deallocate_ele_length
          if (ip .lt. norg_domain) call deallocate_ref_1d_moment
        end if
!
        call deallocate_element_connection
      end do
!
      end subroutine const_filter_moms_newdomain
!
!   --------------------------------------------------------------------
!
      end module trans_filter_moms_newdomain
