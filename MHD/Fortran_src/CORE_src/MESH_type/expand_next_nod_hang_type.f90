!expand_next_nod_hang_type.f90
!      module expand_next_nod_hang_type
!
!> @brief Neighbouring node list for each node
!
!      Written by H.Matsui on Oct., 2006
!
!      subroutine const_next_nod_hang_type(mesh, nod_hang, neib_nod,    &
!     &          neib_hang)
!        type(mesh_geometry), intent(in) :: mesh
!        type(hangining_list), intent(in) :: nod_hang
!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!        type(next_nod_id_4_nod), intent(inout) :: neib_hang
!
!      subroutine overwrt_next_nod_by_hang_type(mesh, neib_hang,        &
!     &          neib_nod)
!        type(mesh_geometry), intent(in) :: mesh
!        type(next_nod_id_4_nod), intent(in) :: neib_hang
!        type(next_nod_id_4_nod), intent(inout) :: neib_nod
!
      module expand_next_nod_hang_type
!
      use m_precision
!
      use m_constants
      use t_mesh_data
      use t_next_node_ele_4_node
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_next_nod_hang_type(mesh, nod_hang, neib_nod,     &
     &          neib_hang)
!
      use t_hanging_mesh_data
      use cal_minmax_and_stacks
      use set_next_node_w_hanging
!
      type(mesh_geometry), intent(in) :: mesh
      type(hangining_list), intent(in) :: nod_hang
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(next_nod_id_4_nod), intent(inout) :: neib_hang
!
!
      call alloc_num_next_node_type(mesh%node%numnod, neib_hang)
      call allocate_iflag_nod_hang(mesh%node%numnod)
!
      call count_next_node_w_hanging(mesh%node%numnod,                  &
     &          nod_hang%iflag_hang,  nod_hang%n_sf, nod_hang%id_sf,    &
     &          nod_hang%n_ed, nod_hang%id_ed,                          &
     &          neib_nod%ntot, neib_nod%istack_next,                    &
     &          neib_nod%inod_next, neib_hang%nnod_next)
!
      call s_cal_minmax_and_stacks(mesh%node%numnod,                    &
     &    neib_hang%nnod_next, izero, neib_hang%istack_next,            &
     &    neib_hang%ntot, neib_hang%nmax, neib_hang%nmin)
!
      call alloc_inod_next_node_type(neib_hang)
!
      call s_set_next_node_w_hanging(mesh%node%numnod,                  &
     &          nod_hang%iflag_hang, nod_hang%n_sf, nod_hang%id_sf,     &
     &          nod_hang%n_ed, nod_hang%id_ed,                          &
     &          neib_nod%ntot, neib_nod%istack_next,                    &
     &          neib_nod%inod_next, neib_hang%ntot,                     &
     &          neib_hang%istack_next, neib_hang%inod_next)
!
      call deallocate_iflag_nod_hang
!
      end subroutine const_next_nod_hang_type
!
!-----------------------------------------------------------------------
!
      subroutine overwrt_next_nod_by_hang_type(mesh, neib_hang,         &
     &          neib_nod)
!
      type(mesh_geometry), intent(in) :: mesh
      type(next_nod_id_4_nod), intent(in) :: neib_hang
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
!
!
      call dealloc_inod_next_node_type(neib_nod)
!
      neib_nod%ntot = neib_hang%ntot
      neib_nod%nmin = neib_hang%nmin
      neib_nod%nmax = neib_hang%nmax
!
      call alloc_num_next_node_type(mesh%node%numnod, neib_nod)
      call alloc_inod_next_node_type(neib_nod)
!
      neib_nod%nnod_next(1:mesh%node%numnod)                            &
     &      = neib_hang%nnod_next(1:mesh%node%numnod)
      neib_nod%istack_next(0:mesh%node%numnod)                          &
     &      = neib_hang%istack_next(0:mesh%node%numnod)
!
      neib_nod%inod_next(1:neib_nod%ntot)                               &
     &      = neib_hang%inod_next(1:neib_nod%ntot) 
      neib_nod%iweight_next(1:neib_nod%ntot)                            &
     &      = neib_hang%iweight_next(1:neib_nod%ntot) 
!
      end subroutine overwrt_next_nod_by_hang_type
!
!-----------------------------------------------------------------------
!
      end module expand_next_nod_hang_type
