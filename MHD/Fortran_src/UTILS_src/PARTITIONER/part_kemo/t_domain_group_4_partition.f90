!>@file  t_domain_group_4_partition.f90
!!       module t_domain_group_4_partition
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2007
!
!> @brief  Domain grouping data for partitioner
!!
!!@verbatim
!!      subroutine alloc_finer_domain_group(f_grp)
!!        type(finer_domain_group), intent(inout) :: f_grp
!!
!!      subroutine dealloc_domain_nod_group(domain_grp)
!!      subroutine dealloc_domain_se_group(domain_grp)
!!        type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!!      subroutine dealloc_local_ne_id_tbl(domain_grp)
!!      subroutine dealloc_local_se_id_tbl
!!        type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!!      subroutine dealloc_org_gl_ne_id(domain_grp)
!!      subroutine dealloc_finer_domain_group(f_grp)
!!        type(finer_domain_group), intent(inout) :: f_grp
!!
!!      subroutine alloc_work_4_rcb(nnod)
!!      subroutine dealloc_work_4_rcb
!!@endverbatim
!
      module t_domain_group_4_partition
!
      use m_precision
!
      implicit none
!
      type domain_group_4_partition
        integer(kind = kint) :: num_s_domin
        integer(kind = kint), allocatable :: IGROUP(:)
        integer(kind = kint), allocatable :: id_local_part(:)
        integer(kind = kint_gl), allocatable :: id_global_org(:)
!
        integer(kind = kint), allocatable :: imark(:)
      end type domain_group_4_partition
!
      type finer_domain_group
        integer(kind = kint) :: nnod_group_finer
        integer(kind = kint),  allocatable :: IGROUP_FINER(:)
      end type finer_domain_group
!
      type partitioner_comm_params
        real(kind=kreal), allocatable :: VAL(:)
        integer(kind=kint), allocatable :: IS1(:)
      end type partitioner_comm_params
!
      type domain_groups_4_partitioner
        integer(kind = kint) :: intnod_s_domin
!
        type(domain_group_4_partition)  :: nod_d_grp
        type(domain_group_4_partition)  :: ele_d_grp
!
        type(domain_group_4_partition)  :: surf_d_grp
        type(domain_group_4_partition)  :: edge_d_grp
!
        type(finer_domain_group) :: nod_f_grp
      end type domain_groups_4_partitioner
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_domain_group(d_grp)
!
      type(domain_group_4_partition), intent(inout) :: d_grp
!
      allocate(d_grp%IGROUP(d_grp%num_s_domin))
      if(d_grp%num_s_domin .gt. 0) d_grp%IGROUP = 0
!
      end subroutine alloc_domain_group
!
!   --------------------------------------------------------------------
!
      subroutine alloc_local_id_tbl(d_grp)
!
      type(domain_group_4_partition), intent(inout) :: d_grp
!
      allocate(d_grp%id_local_part(d_grp%num_s_domin))
      if(d_grp%num_s_domin .gt. 0) d_grp%id_local_part =  0
!
      end subroutine alloc_local_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine alloc_org_gl_id(d_grp)
!
      type(domain_group_4_partition), intent(inout) :: d_grp
!
      allocate(d_grp%id_global_org(d_grp%num_s_domin))
      if(d_grp%num_s_domin .gt. 0) d_grp%id_global_org = 0
!
      end subroutine alloc_org_gl_id
!
!   --------------------------------------------------------------------
!
      subroutine alloc_domain_group_imark(d_grp)
!
      type(domain_group_4_partition), intent(inout) :: d_grp
!
      allocate(d_grp%imark(d_grp%num_s_domin))
      if(d_grp%num_s_domin .gt. 0) d_grp%imark = 0
!
      end subroutine alloc_domain_group_imark
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_domain_group(d_grp)
!
      type(domain_group_4_partition), intent(inout) :: d_grp
!
      deallocate(d_grp%IGROUP)
!
      end subroutine dealloc_domain_group
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_local_id_tbl(d_grp)
!
      type(domain_group_4_partition), intent(inout) :: d_grp
!
      deallocate(d_grp%id_local_part)
!
      end subroutine dealloc_local_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_org_gl_id(d_grp)
!
      type(domain_group_4_partition), intent(inout) :: d_grp
!
      deallocate(d_grp%id_global_org)
!
      end subroutine dealloc_org_gl_id
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_domain_group_imark(d_grp)
!
      type(domain_group_4_partition), intent(inout) :: d_grp
!
      deallocate(d_grp%imark)
!
      end subroutine dealloc_domain_group_imark
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_finer_domain_group(f_grp)
!
      type(finer_domain_group), intent(inout) :: f_grp
!
      allocate(f_grp%IGROUP_FINER(f_grp%nnod_group_finer))
      if(f_grp%nnod_group_finer .gt. 0) f_grp%IGROUP_FINER = 0
!
      end subroutine alloc_finer_domain_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_domain_nod_group(domain_grp)
!
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!
      call dealloc_domain_group(domain_grp%nod_d_grp)
      call dealloc_domain_group(domain_grp%ele_d_grp)
!
      end subroutine dealloc_domain_nod_group
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_domain_se_group(domain_grp)
!
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!
!
      call dealloc_domain_group(domain_grp%surf_d_grp)
      call dealloc_domain_group(domain_grp%edge_d_grp)
!
      end subroutine dealloc_domain_se_group
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_local_ne_id_tbl(domain_grp)
!
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!
      call dealloc_local_id_tbl(domain_grp%nod_d_grp)
      call dealloc_local_id_tbl(domain_grp%ele_d_grp)
!
      end subroutine dealloc_local_ne_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_local_se_id_tbl(domain_grp)
!
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!
!
      call dealloc_local_id_tbl(domain_grp%surf_d_grp)
      call dealloc_local_id_tbl(domain_grp%edge_d_grp)
!
      end subroutine dealloc_local_se_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_org_gl_ne_id(domain_grp)
!
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!
      call dealloc_org_gl_id(domain_grp%nod_d_grp)
!      call dealloc_org_gl_id(domain_grp%ele_d_grp)
!
      end subroutine dealloc_org_gl_ne_id
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_finer_domain_group(f_grp)
!
      type(finer_domain_group), intent(inout) :: f_grp
!
      deallocate(f_grp%IGROUP_FINER)
!
      end subroutine dealloc_finer_domain_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_work_4_rcb(nnod, part_comm)
!
      integer(kind = kint), intent(in) :: nnod
      type(partitioner_comm_params), intent(inout) :: part_comm
!
      allocate(part_comm%VAL(nnod))
      allocate(part_comm%IS1(nnod))
!
      if(nnod .eq. 0) return
!
!$omp parallel workshare
      part_comm%VAL = 0.0d0
      part_comm%IS1 = 0
!$omp end parallel workshare
!
      end subroutine alloc_work_4_rcb
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_work_4_rcb(part_comm)
!
      type(partitioner_comm_params), intent(inout) :: part_comm
!
      deallocate (part_comm%VAL)
      deallocate (part_comm%IS1)
!
      end subroutine dealloc_work_4_rcb
!
!   --------------------------------------------------------------------
!
      end module t_domain_group_4_partition
