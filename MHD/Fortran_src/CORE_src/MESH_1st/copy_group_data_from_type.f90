!>@file   copy_group_data_from_type.f90
!!@brief  module copy_group_data_from_type
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!
!>@brief Copy group data from structure to 1st mesh modules
!!
!!@verbatim
!!      subroutine group_data_from_type(group)
!!      subroutine compare_group_type_vs_1st(my_rank, group)
!!        type(mesh_groups), intent(inout) :: group
!!@endverbatim
!
      module copy_group_data_from_type
!
      use m_precision
!
      implicit  none
!
      private ::  compare_nod_grp_type_vs_1st
      private ::  compare_ele_grp_type_vs_1st
      private ::  compare_surf_grp_type_vs_1st
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine group_data_from_type(group)
!
      use m_group_data
      use t_mesh_data
      use t_group_data
!
      type(mesh_groups), intent(inout) :: group
!
!
      call copy_group_data(group%nod_grp, nod_grp1)
      call copy_group_data(group%ele_grp, ele_grp1)
      call copy_surface_group(group%surf_grp, sf_grp1)
!
      call dealloc_groups_data(group)
!
      end subroutine group_data_from_type
!
!-----------------------------------------------------------------------
!
      subroutine compare_group_type_vs_1st(my_rank, group_ref)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_groups), intent(in) :: group_ref
!
!
      call compare_nod_grp_type_vs_1st(my_rank, group_ref%nod_grp)
      call compare_ele_grp_type_vs_1st(my_rank, group_ref%ele_grp)
      call compare_surf_grp_type_vs_1st(my_rank, group_ref%surf_grp)
!
      end subroutine compare_group_type_vs_1st
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine compare_nod_grp_type_vs_1st(my_rank, nod_grp_ref)
!
      use m_node_group
      use t_group_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(group_data), intent(in) :: nod_grp_ref
!
      integer(kind = kint) :: i
!
!
      if(nod_grp_ref%num_grp .ne. nod_grp1%num_grp) write(*,*)          &
     &   'num_bc', my_rank, nod_grp_ref%num_grp, nod_grp1%num_grp
      if(nod_grp_ref%num_item .ne. nod_grp1%num_item) write(*,*)        &
     &   'num_nod_bc', my_rank, nod_grp_ref%num_item, nod_grp1%num_item
      do i = 1, nod_grp1%num_grp
        if(nod_grp_ref%grp_name(i) .ne. nod_grp1%grp_name(i))           &
     &       write(*,*) 'bc_name(i)', my_rank, i,                       &
     &       nod_grp_ref%grp_name(i), nod_grp1%grp_name(i)
        if(nod_grp_ref%istack_grp(i) .ne. nod_grp1%istack_grp(i))       &
     &       write(*,*) 'bc_istack(i)', my_rank, i,                     &
     &       nod_grp_ref%istack_grp(i), nod_grp1%istack_grp(i)
      end do
      do i = 1, nod_grp1%num_item
        if(nod_grp_ref%item_grp(i) .ne. nod_grp1%item_grp(i))           &
     &       write(*,*) 'bc_item(i)', my_rank, i,                       &
     &       nod_grp_ref%item_grp(i), nod_grp1%item_grp(i)
      end do
!
      end subroutine compare_nod_grp_type_vs_1st
!
!-----------------------------------------------------------------------
!
      subroutine compare_ele_grp_type_vs_1st(my_rank, ele_grp_ref)
!
      use m_element_group
      use t_group_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(group_data), intent(in) :: ele_grp_ref
!
      integer(kind = kint) :: i
!
      if(ele_grp_ref%num_grp .ne. ele_grp1%num_grp) write(*,*)          &
     &   'num_mat', my_rank, ele_grp_ref%num_grp, ele_grp1%num_grp
      if(ele_grp_ref%num_item .ne. ele_grp1%num_item) write(*,*)        &
     &   'num_mat_bc', my_rank, ele_grp_ref%num_item, ele_grp1%num_item
      do i = 1, ele_grp1%num_grp
        if(ele_grp_ref%grp_name(i) .ne. ele_grp1%grp_name(i))           &
     &       write(*,*) 'mat_name(i)', my_rank, i,                      &
     &       ele_grp_ref%grp_name(i), ele_grp1%grp_name(i)
        if(ele_grp_ref%istack_grp(i) .ne. ele_grp1%istack_grp(i))       &
     &       write(*,*) 'mat_istack(i)', my_rank, i,                    &
     &       ele_grp_ref%istack_grp(i), ele_grp1%istack_grp(i)
      end do
      do i = 1, ele_grp1%num_item
        if(ele_grp_ref%item_grp(i) .ne. ele_grp1%item_grp(i))           &
     &       write(*,*) 'mat_item(i)', my_rank, i,                      &
     &       ele_grp_ref%item_grp(i), ele_grp1%item_grp(i)
      end do
!
      end subroutine compare_ele_grp_type_vs_1st
!
!-----------------------------------------------------------------------
!
      subroutine compare_surf_grp_type_vs_1st(my_rank, sf_grp_ref)
!
      use m_surface_group
      use t_group_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(surface_group_data), intent(in) :: sf_grp_ref
!
      integer(kind = kint) :: i
!
!
      if(sf_grp_ref%num_grp .ne. sf_grp1%num_grp) write(*,*)            &
     &   'num_surf', my_rank, sf_grp_ref%num_grp, sf_grp1%num_grp
      if(sf_grp_ref%num_item .ne. sf_grp1%num_item) write(*,*)          &
     &   'num_surf_bc', my_rank, sf_grp_ref%num_item, sf_grp1%num_item
      do i = 1, sf_grp1%num_grp
        if(sf_grp_ref%grp_name(i) .ne. sf_grp1%grp_name(i))             &
     &       write(*,*) 'surf_name(i)', my_rank, i,                     &
     &       sf_grp_ref%grp_name(i), sf_grp1%grp_name(i)
        if(sf_grp_ref%istack_grp(i) .ne. sf_grp1%istack_grp(i))         &
     &       write(*,*) 'surf_istack(i)', my_rank, i,                   &
     &       sf_grp_ref%istack_grp(i), sf_grp1%istack_grp(i)
      end do
      do i = 1, sf_grp1%num_item
        if(sf_grp_ref%item_sf_grp(1,i) .ne. sf_grp1%item_sf_grp(1,i)    &
     &  .or. sf_grp_ref%item_sf_grp(2,i) .ne. sf_grp1%item_sf_grp(2,i)) &
     &       write(*,*) 'surf_item(:,i)', my_rank, i,                   &
     &       sf_grp_ref%item_sf_grp(1,i), sf_grp1%item_sf_grp(1,i),     &
     &       sf_grp_ref%item_sf_grp(2,i), sf_grp1%item_sf_grp(2,i)
      end do
!
      end subroutine compare_surf_grp_type_vs_1st
!
!-----------------------------------------------------------------------
!
      end module copy_group_data_from_type
