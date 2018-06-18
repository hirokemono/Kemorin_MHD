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
!!        type(parallel_double_numbering), intent(inout) :: dbl_id
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
      use new_SPH_restart
      use sph_file_MPI_IO_select
!
      integer(kind = kint), intent(in) :: ip_org
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: irank_org
!
!
        irank_org = mod(ip_org - 1,nprocs)
!        write(*,*) 'MPI_Bcast num_neib', ip_org
        call MPI_Bcast(nod_comm%num_neib, ione, CALYPSO_INTEGER,        &
     &      irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(nod_comm%ntot_import, ione, CALYPSO_INTEGER,     &
     &      irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(nod_comm%ntot_export, ione, CALYPSO_INTEGER,     &
     &      irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(irank_org .ne. my_rank) then
        call allocate_type_comm_tbl_num(nod_comm)
        call allocate_type_comm_tbl_item(nod_comm)
      end if
!
!      write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(nod_comm%id_neib, nod_comm%num_neib,               &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nod_comm%istack_import, nod_comm%num_neib,         &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nod_comm%istack_export, nod_comm%num_neib,         &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(nod_comm%item_import, nod_comm%ntot_import,        &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nod_comm%item_export, nod_comm%ntot_export,        &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_each_comm_table
!
! -----------------------------------------------------------------------
!
      subroutine share_each_node_data(ip_org, node)
!
      integer(kind = kint), intent(in) :: ip_org
      type(node_data), intent(inout) :: node
!
      integer(kind = kint) :: irank_org
!
!
      irank_org = mod(ip_org - 1,nprocs)
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(node%numnod, ione, CALYPSO_INTEGER,                &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(node%internal_node, ione, CALYPSO_INTEGER,         &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        call alloc_node_geometry_base(node)
      end if
!
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(node%inod_global, node%numnod, CALYPSO_GLOBAL_INT, &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(node%xx, (3*node%numnod), CALYPSO_REAL,            &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_each_node_data
!
! -----------------------------------------------------------------------
!
      subroutine share_each_element_data(ip_org, ele)
!
      integer(kind = kint), intent(in) :: ip_org
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: irank_org, num
!
!
      irank_org = mod(ip_org - 1,nprocs)
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(ele%numele, ione, CALYPSO_INTEGER,                 &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ele%first_ele_type, ione, CALYPSO_INTEGER,         &
     &     irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        call set_nnod_4_ele_by_eletype                                  &
     &     (ele%first_ele_type, ele%nnod_4_ele)
        call allocate_ele_connect_type(ele)
      end if
!
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(ele%iele_global, ele%numele,  CALYPSO_GLOBAL_INT,  &
     &     irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ele%elmtyp, ele%numele,  CALYPSO_INTEGER,          &
     &     irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ele%nodelm, ele%numele,  CALYPSO_INTEGER,          &
     &     irank_org, CALYPSO_COMM, ierr_MPI)
!
      num = ele%numele * ele%nnod_4_ele
      call MPI_Bcast(ele%nodelm, num,  CALYPSO_INTEGER,                 &
     &     irank_org, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_each_element_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_each_group_data(ip_org, group)
!
      integer(kind = kint), intent(in) :: ip_org
      type(group_data), intent(inout) :: group
!
      integer(kind = kint) :: irank_org
!
!
      irank_org = mod(ip_org - 1,nprocs)
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(group%num_grp, ione, CALYPSO_INTEGER,              &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(group%num_item, ione, CALYPSO_INTEGER,             &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        call allocate_grp_type_num(group)
        call allocate_grp_type_item(group)
      end if
!
      if(group%num_grp .gt. 0) then
        call MPI_Bcast(group%istack_grp, (group%num_grp+1),             &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(group%nitem_grp, group%num_grp,                  &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(group%grp_name, (group%num_grp*kchara),          &
     &      CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      end if
!
      if(group%num_item .gt. 0) then
        call MPI_Bcast(group%item_grp, group%num_item,                  &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      end if
!
      end subroutine share_each_group_data
!
! -----------------------------------------------------------------------
!
      subroutine share_each_surf_group_data(ip_org, sf_group)
!
      integer(kind = kint), intent(in) :: ip_org
      type(surface_group_data), intent(inout) :: sf_group
!
      integer(kind = kint) :: irank_org
!
!
      irank_org = mod(ip_org - 1,nprocs)
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(sf_group%num_grp, ione, CALYPSO_INTEGER,           &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(sf_group%num_item, ione, CALYPSO_INTEGER,          &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        call allocate_sf_grp_type_num(sf_group)
        call allocate_sf_grp_type_item(sf_group)
      end if
!
      if(sf_group%num_grp .gt. 0) then
        call MPI_Bcast(sf_group%istack_grp, (sf_group%num_grp+1),       &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(sf_group%nitem_grp, sf_group%num_grp,            &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(sf_group%grp_name, (sf_group%num_grp*kchara),    &
     &      CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      end if
!
      if(sf_group%num_item .gt. 0) then
        call MPI_Bcast(sf_group%item_sf_grp, (2*sf_group%num_item),     &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      end if
!
      end subroutine share_each_surf_group_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_doble_numbering(ip_org, dbl_id)
!
      integer(kind = kint), intent(in) :: ip_org
      type(parallel_double_numbering), intent(inout) :: dbl_id
!
      integer(kind = kint) :: irank_org, num
!
!
      irank_org = mod(ip_org - 1,nprocs)
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(dbl_id%nnod_local, ione, CALYPSO_INTEGER,          &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        num = dbl_id%nnod_local
        call alloc_double_numbering(num, dbl_id)
      end if
!
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(dbl_id%inod_local, dbl_id%nnod_local,              &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(dbl_id%irank_home, dbl_id%nnod_local,              &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_doble_numbering
!
! -----------------------------------------------------------------------
!
      end module share_mesh_data
