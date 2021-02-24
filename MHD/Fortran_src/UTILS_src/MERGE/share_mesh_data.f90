!>@file   share_mesh_data.f90
!!@brief  module share_mesh_data
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2011
!
!>@brief Construct spectrum data for new spectrum domain
!!
!!@verbatim
!!      subroutine share_each_comm_table(ip_org, nod_comm)
!!        type(communication_table), intent(inout) :: nod_comm
!!      subroutine share_each_node_data(ip_org, node)
!!        type(node_data), intent(inout) :: node
!!      subroutine share_each_element_data(ip_org, ele)
!!        type(element_data), intent(inout) :: ele
!!
!!      subroutine share_each_group_data(ip_org, group)
!!        type(group_data), intent(inout) :: group
!!      subroutine share_each_surf_group_data(ip_org, sf_group)
!!        type(surface_group_data), intent(inout) :: sf_group
!!
!!      subroutine share_doble_numbering(ip_org, dbl_id)
!!        type(node_ele_double_number), intent(inout) :: dbl_id
!!@endverbatim
!
      module share_mesh_data
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use t_geometry_data
      use t_comm_table
      use t_group_data
      use t_para_double_numbering
!
      implicit none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine share_each_comm_table(ip_org, nod_comm)
!
      use calypso_mpi_int
      use new_SPH_restart
      use sph_file_MPI_IO_select
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ip_org
      type(communication_table), intent(inout) :: nod_comm
!
      integer :: irank_org
!
!
        irank_org = int(mod(ip_org - 1,nprocs))
!        write(*,*) 'MPI_Bcast num_neib', ip_org
        call calypso_mpi_bcast_one_int(nod_comm%num_neib, irank_org)
        call calypso_mpi_bcast_one_int                                  &
     &     (nod_comm%ntot_import, irank_org)
        call calypso_mpi_bcast_one_int                                  &
     &     (nod_comm%ntot_export, irank_org)
!
      if(irank_org .ne. my_rank) then
        call alloc_comm_table_num(nod_comm)
        call alloc_comm_table_item(nod_comm)
      end if
!
!      write(*,*) 'MPI_Bcast num_neib', ip_org
      call calypso_mpi_bcast_int                                        &
     &    (nod_comm%id_neib, cast_long(nod_comm%num_neib), irank_org)
      call calypso_mpi_bcast_int(nod_comm%istack_import,                &
     &    cast_long(nod_comm%num_neib), irank_org)
      call calypso_mpi_bcast_int(nod_comm%istack_export,                &
     &    cast_long(nod_comm%num_neib), irank_org)
!
      call calypso_mpi_bcast_int(nod_comm%item_import,                  &
     &    cast_long(nod_comm%ntot_import), irank_org)
      call calypso_mpi_bcast_int(nod_comm%item_export,                  &
     &    cast_long(nod_comm%ntot_export), irank_org)
!
      end subroutine share_each_comm_table
!
! -----------------------------------------------------------------------
!
      subroutine share_each_node_data(ip_org, node)
!
      use calypso_mpi_int
      use calypso_mpi_int8
      use calypso_mpi_real
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ip_org
      type(node_data), intent(inout) :: node
!
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org - 1,nprocs))
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call calypso_mpi_bcast_one_int(node%numnod, irank_org)
      call calypso_mpi_bcast_one_int(node%internal_node, irank_org)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        call alloc_node_geometry_base(node)
      end if
!
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call calypso_mpi_bcast_int8                                       &
     &   (node%inod_global, cast_long(node%numnod), irank_org)
      call calypso_mpi_bcast_real                                       &
     &   (node%xx, cast_long(3*node%numnod), irank_org)
!
      end subroutine share_each_node_data
!
! -----------------------------------------------------------------------
!
      subroutine share_each_element_data(ip_org, ele)
!
      use calypso_mpi_int
      use calypso_mpi_int8
      use set_nnod_4_ele_by_type
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ip_org
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: num
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org - 1,nprocs))
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call calypso_mpi_bcast_one_int(ele%numele, irank_org)
      call calypso_mpi_bcast_one_int(ele%first_ele_type, irank_org)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        ele%nnod_4_ele = set_nnod_4_ele_by_eletype(ele%first_ele_type)
        call alloc_ele_connect(ele)
      end if
!
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call calypso_mpi_bcast_int8                                       &
     &    (ele%iele_global, cast_long(ele%numele), irank_org)
      call calypso_mpi_bcast_int                                        &
     &    (ele%elmtyp, cast_long(ele%numele), irank_org)
      call calypso_mpi_bcast_int                                        &
     &   (ele%nodelm, cast_long(ele%numele), irank_org)
!
      num = ele%numele * ele%nnod_4_ele
      call calypso_mpi_bcast_int(ele%ie, cast_long(num), irank_org)
!
      end subroutine share_each_element_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_each_group_data(ip_org, group)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ip_org
      type(group_data), intent(inout) :: group
!
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org - 1,nprocs))
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call calypso_mpi_bcast_one_int(group%num_grp, irank_org)
      call calypso_mpi_bcast_one_int(group%num_item, irank_org)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        call alloc_group_num(group)
        call alloc_group_item(group)
      end if
!
      if(group%num_grp .gt. 0) then
        call calypso_mpi_bcast_int                                      &
     &     (group%istack_grp, cast_long(group%num_grp+1), irank_org)
        call calypso_mpi_bcast_int                                      &
     &     (group%nitem_grp, cast_long(group%num_grp), irank_org)
        call calypso_mpi_bcast_character                                &
     &     (group%grp_name, cast_long(group%num_grp*kchara), 0)
      end if
!
      if(group%num_item .gt. 0) then
        call calypso_mpi_bcast_int                                      &
     &     (group%item_grp, cast_long(group%num_item), irank_org)
      end if
!
      end subroutine share_each_group_data
!
! -----------------------------------------------------------------------
!
      subroutine share_each_surf_group_data(ip_org, sf_group)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ip_org
      type(surface_group_data), intent(inout) :: sf_group
!
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org - 1,nprocs))
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call calypso_mpi_bcast_one_int(sf_group%num_grp, irank_org)
      call calypso_mpi_bcast_one_int(sf_group%num_item, irank_org)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        call alloc_sf_group_num(sf_group)
        call alloc_sf_group_item(sf_group)
      end if
!
      if(sf_group%num_grp .gt. 0) then
        call calypso_mpi_bcast_int(sf_group%istack_grp,                 &
     &      cast_long(sf_group%num_grp+1), irank_org)
        call calypso_mpi_bcast_int(sf_group%nitem_grp,                  &
     &      cast_long(sf_group%num_grp), irank_org)
        call calypso_mpi_bcast_character(sf_group%grp_name,             &
     &      cast_long(sf_group%num_grp*kchara), 0)
      end if
!
      if(sf_group%num_item .gt. 0) then
        call calypso_mpi_bcast_int(sf_group%item_sf_grp,                &
     &      cast_long(2*sf_group%num_item), irank_org)
      end if
!
      end subroutine share_each_surf_group_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_doble_numbering(ip_org, dbl_id)
!
      use calypso_mpi_int
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ip_org
      type(node_ele_double_number), intent(inout) :: dbl_id
!
      integer(kind = kint) :: num
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org - 1,nprocs))
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call calypso_mpi_bcast_one_int(dbl_id%num_dbl, irank_org)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        num = dbl_id%num_dbl
        call alloc_double_numbering(num, dbl_id)
      end if
!
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call calypso_mpi_bcast_int                                        &
     &   (dbl_id%id_local, cast_long(dbl_id%num_dbl), irank_org)
      call calypso_mpi_bcast_int                                        &
     &   (dbl_id%ip_home, cast_long(dbl_id%num_dbl), irank_org)
!
      end subroutine share_doble_numbering
!
! -----------------------------------------------------------------------
!
      end module share_mesh_data
