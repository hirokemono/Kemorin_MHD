!bcast_nodes_for_trans.f90
!      module bcast_nodes_for_trans
!
!      Written by H. Matsui on May, 2008
!
!!      subroutine bcast_parallel_domain_tbl(mesh_file, nod_d_grp)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(domain_group_4_partition), intent(inout)  :: nod_d_grp
!!
!!      subroutine bcast_num_filter_part_table(nprocs_2nd, itl_nod_part)
!!      subroutine bcast_xx_whole_nod(nnod_global, itl_nod_part)
!         type(internal_4_partitioner), intent(inout)  :: itl_nod_part
!
      module bcast_nodes_for_trans
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_work_time
      use t_internal_4_partitioner
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
      subroutine bcast_parallel_domain_tbl(mesh_file, nod_d_grp)
!
      use t_file_IO_parameter
      use m_2nd_pallalel_vector
      use t_domain_group_4_partition
      use const_domain_tbl_by_file
!
      type(field_IO_params), intent(in) :: mesh_file
      type(domain_group_4_partition), intent(inout)  :: nod_d_grp
!
!
      if(my_rank .eq. 0) then
        call count_nnod_whole_domain(mesh_file, nod_d_grp)
      end if
!
      if(iflag_F3D_time) call start_elapsed_time(ist_elapsed_F3D+1)
      call MPI_Bcast(nod_d_grp%num_s_domin, 1, CALYPSO_INTEGER,         &
     &    0, CALYPSO_COMM, ierr_MPI)
      if(iflag_F3D_time) call end_elapsed_time(ist_elapsed_F3D+1)
!
!
      call alloc_domain_group(nod_d_grp)
      call alloc_local_id_tbl(nod_d_grp)
      call alloc_org_gl_id(nod_d_grp)
!
      if(iflag_F3D_time) call start_elapsed_time(ist_elapsed_F3D+2)
      if(my_rank .eq. 0) then
        call set_domain_grp_whole_domain(mesh_file, nod_d_grp)
      else
        call set_domain_grp_each_domain(mesh_file, my_rank, nod_d_grp)
      end if
      if(iflag_F3D_time) call end_elapsed_time(ist_elapsed_F3D+2)
!
      end subroutine bcast_parallel_domain_tbl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_num_filter_part_table(nprocs_2nd, itl_nod_part)
!
      use set_filters_4_new_domains
!
      integer(kind = kint), intent(in) :: nprocs_2nd
      type(internal_4_partitioner), intent(inout)  :: itl_nod_part
!
!
      call MPI_Bcast(itl_nod_part%num_inter_sub(1), nprocs_2nd,         &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(itl_nod_part%istack_inter_sub(1), nprocs_2nd,      &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(itl_nod_part%ntot_inter_sub,  1,                   &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(itl_nod_part%nmax_inter_sub,  1,                   &
     &    CALYPSO_INTEGER,  0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(itl_nod_part%nmin_inter_sub,  1,                   &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(itl_nod_part%num_4_subdomain(1), nprocs_2nd,       &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(itl_nod_part%istack_4_subdomain(1),  nprocs_2nd,   &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(itl_nod_part%ntot_sub,  1, CALYPSO_INTEGER,        &
     &    0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(itl_nod_part%nmax_sub,  1, CALYPSO_INTEGER,        &
     &    0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(itl_nod_part%nmin_sub,  1, CALYPSO_INTEGER,        &
     &    0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_num_filter_part_table
!
!   --------------------------------------------------------------------
!
      subroutine bcast_xx_whole_nod(nnod_global, itl_nod_part)
!
      use set_filters_4_new_domains
!
      integer(kind = kint), intent(in) :: nnod_global
      type(internal_4_partitioner), intent(inout)  :: itl_nod_part
      integer(kind = kint) :: num
!
!
      num = 3 * nnod_global
!
      call MPI_Bcast(xx_whole_nod(1,1), num, CALYPSO_REAL,              &
     &    0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(itl_nod_part%id_inter_subdomain(1),                &
     &    itl_nod_part%ntot_inter_sub, CALYPSO_INTEGER, 0,              &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast                                                    &
     &   (itl_nod_part%id_4_subdomain(1), itl_nod_part%ntot_sub,        &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_xx_whole_nod
!
!   --------------------------------------------------------------------
!
      end module bcast_nodes_for_trans
