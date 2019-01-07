!bcast_nodes_for_trans.f90
!      module bcast_nodes_for_trans
!
!      Written by H. Matsui on May, 2008
!
!!      subroutine bcast_parallel_domain_tbl(mesh_file)
!!        type(field_IO_params), intent(in) :: mesh_file
!!
!!      subroutine bcast_num_filter_part_table(nprocs_2nd)
!!      subroutine bcast_xx_whole_nod(nnod_global)
!
      module bcast_nodes_for_trans
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_internal_4_partitioner
      use m_work_time
!
      implicit none
!
      logical, save, private :: iflag_F3D_time = .FALSE.
      integer(kind = kint), save, private :: ist_elapsed_F3D = 0
      integer(kind = kint), save, private :: ied_elapsed_F3D = 0
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine elpsed_label_3dfilter
!
      integer(kind = kint), parameter :: num_append = 2
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_F3D, ied_elapsed_F3D)
!
      elps1%labels(ist_elapsed_F3D+1) = 'count total node time'
      elps1%labels(ist_elapsed_F3D+2) = 'set domain table time'
!
      iflag_F3D_time = .TRUE.
!
      end subroutine elpsed_label_3dfilter
!
!   --------------------------------------------------------------------
!
      subroutine bcast_parallel_domain_tbl(mesh_file)
!
      use t_file_IO_parameter
      use m_2nd_pallalel_vector
      use m_domain_group_4_partition
      use const_domain_tbl_by_file
!
      type(field_IO_params), intent(in) :: mesh_file
!
!
      if(my_rank .eq. 0) then
        call count_nnod_whole_domain(mesh_file, nod_d_grp1)
        ele_d_grp1%num_s_domin = 0
        surf_d_grp1%num_s_domin = 0
        edge_d_grp1%num_s_domin = 0
      end if
!
      if(iflag_F3D_time) call start_elapsed_time(ist_elapsed_F3D+1)
      call MPI_Bcast(nod_d_grp1%num_s_domin, ione, CALYPSO_INTEGER,     &
     &    izero, CALYPSO_COMM, ierr_MPI)
      if(iflag_F3D_time) call end_elapsed_time(ist_elapsed_F3D+1)
!
!
      call alloc_domain_group(nod_d_grp1)
      call alloc_local_id_tbl(nod_d_grp1)
      call alloc_org_gl_id(nod_d_grp1)
      call alloc_domain_group(ele_d_grp1)
      call alloc_local_id_tbl(ele_d_grp1)
      call alloc_domain_group(surf_d_grp1)
      call alloc_local_id_tbl(surf_d_grp1)
      call alloc_domain_group(edge_d_grp1)
      call alloc_local_id_tbl(edge_d_grp1)
!
      if(iflag_F3D_time) call start_elapsed_time(ist_elapsed_F3D+2)
      if(my_rank .eq. 0) then
        call set_domain_grp_whole_domain(mesh_file, nod_d_grp1)
      else
        call set_domain_grp_each_domain(mesh_file, my_rank, nod_d_grp1)
      end if
      if(iflag_F3D_time) call end_elapsed_time(ist_elapsed_F3D+2)
!
      end subroutine bcast_parallel_domain_tbl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_num_filter_part_table(nprocs_2nd)
!
      use set_filters_4_new_domains
!
      integer(kind = kint), intent(in) :: nprocs_2nd
!
!
      call MPI_Bcast(num_intnod_sub(1), nprocs_2nd, CALYPSO_INTEGER,    &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(istack_intnod_sub(1), nprocs_2nd, CALYPSO_INTEGER, &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ntot_intnod_sub,  ione, CALYPSO_INTEGER,           &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nmax_intnod_sub,  ione, CALYPSO_INTEGER,           &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nmin_intnod_sub,  ione, CALYPSO_INTEGER,           &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(numnod_4_subdomain(1), nprocs_2nd,                 &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(istack_numnod_sub(1),  nprocs_2nd,                 &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ntot_numnod_sub,  ione, CALYPSO_INTEGER,           &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nmax_numnod_sub,  ione, CALYPSO_INTEGER,           &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nmin_numnod_sub,  ione, CALYPSO_INTEGER,           &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_num_filter_part_table
!
!   --------------------------------------------------------------------
!
      subroutine bcast_xx_whole_nod(nnod_global)
!
      use set_filters_4_new_domains
!
      integer(kind = kint), intent(in) :: nnod_global
      integer(kind = kint) :: num
!
!
      num = 3 * nnod_global
!
      call MPI_Bcast(xx_whole_nod(1,1), num, CALYPSO_REAL,              &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(inod_intnod_sub(1), ntot_intnod_sub,               &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(inod_4_subdomain(1), ntot_numnod_sub,              &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_xx_whole_nod
!
!   --------------------------------------------------------------------
!
      end module bcast_nodes_for_trans
