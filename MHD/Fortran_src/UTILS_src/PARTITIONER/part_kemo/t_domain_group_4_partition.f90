!>@file  t_domain_group_4_partition.f90
!!       module t_domain_group_4_partition
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2007
!
!> @brief  Domain grouping data for partitioner
!!
!!@verbatim
!!      subroutine alloc_domain_nod_group
!!      subroutine alloc_domain_nese_group
!!      subroutine alloc_local_ne_id_tbl
!!      subroutine alloc_local_nese_id_tbl
!!      subroutine alloc_org_gl_nod_id
!!      subroutine alloc_org_gl_nese_id
!!      subroutine alloc_finer_domain_group
!!
!!      subroutine dealloc_domain_nod_group
!!      subroutine dealloc_domain_nese_group
!!      subroutine dealloc_local_nese_id_tbl
!!      subroutine dealloc_org_gl_ne_id
!!      subroutine dealloc_org_gl_nese_id
!!      subroutine dealloc_finer_domain_group
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
      end type domain_group_4_partition
!
      type(domain_group_4_partition)  :: nod_d_grp1
      type(domain_group_4_partition)  :: ele_d_grp1
      type(domain_group_4_partition)  :: surf_d_grp1
      type(domain_group_4_partition)  :: edge_d_grp1
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
!   --------------------------------------------------------------------
!
      subroutine alloc_domain_nod_group
!
!
      call alloc_domain_group(nod_d_grp1)
      call alloc_domain_group(ele_d_grp1)
!
      end subroutine alloc_domain_nod_group
!
!   --------------------------------------------------------------------
!
      subroutine alloc_domain_nese_group
!
!
      call alloc_domain_group(nod_d_grp1)
      call alloc_domain_group(ele_d_grp1)
      call alloc_domain_group(surf_d_grp1)
      call alloc_domain_group(edge_d_grp1)
!
      end subroutine alloc_domain_nese_group
!
!   --------------------------------------------------------------------
!
      subroutine alloc_local_ne_id_tbl
!
      call alloc_local_id_tbl(nod_d_grp1)
      call alloc_local_id_tbl(ele_d_grp1)
!
      end subroutine alloc_local_ne_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine alloc_local_nese_id_tbl
!
      call alloc_local_id_tbl(nod_d_grp1)
      call alloc_local_id_tbl(ele_d_grp1)
      call alloc_local_id_tbl(surf_d_grp1)
      call alloc_local_id_tbl(edge_d_grp1)
!
      end subroutine alloc_local_nese_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine alloc_org_gl_nod_id
!
      call alloc_org_gl_id(nod_d_grp1)
!
      end subroutine alloc_org_gl_nod_id
!
!   --------------------------------------------------------------------
!
      subroutine alloc_org_gl_nese_id
!
      call alloc_org_gl_nod_id
!
      end subroutine alloc_org_gl_nese_id
!
!   --------------------------------------------------------------------
!
      subroutine alloc_finer_domain_group
!
!      allocate(IGROUP_FINER(nnod_group_finer))
!      IGROUP_FINER = 0
!
      end subroutine alloc_finer_domain_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_domain_nod_group
!
      call dealloc_domain_group(nod_d_grp1)
      call dealloc_domain_group(ele_d_grp1)
!
      end subroutine dealloc_domain_nod_group
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_domain_nese_group
!
      call dealloc_domain_group(nod_d_grp1)
      call dealloc_domain_group(ele_d_grp1)
      call dealloc_domain_group(surf_d_grp1)
      call dealloc_domain_group(edge_d_grp1)
!
      end subroutine dealloc_domain_nese_group
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_local_ne_id_tbl
!
!
      call dealloc_local_id_tbl(nod_d_grp1)
      call dealloc_local_id_tbl(ele_d_grp1)
!
      end subroutine dealloc_local_ne_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_local_nese_id_tbl
!
      call dealloc_local_id_tbl(nod_d_grp1)
      call dealloc_local_id_tbl(ele_d_grp1)
      call dealloc_local_id_tbl(surf_d_grp1)
      call dealloc_local_id_tbl(edge_d_grp1)
!
      end subroutine dealloc_local_nese_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_org_gl_ne_id
!
      call dealloc_org_gl_id(nod_d_grp1)
!      call dealloc_org_gl_id(ele_d_grp1)
!
      end subroutine dealloc_org_gl_ne_id
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_org_gl_nese_id
!
      call dealloc_org_gl_id(nod_d_grp1)
!      call dealloc_org_gl_id(ele_d_grp1)
!      call dealloc_org_gl_id(surf_d_grp1)
!      call dealloc_org_gl_id(edge_d_grp1)
!
      end subroutine dealloc_org_gl_nese_id
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_finer_domain_group
!
!      deallocate(IGROUP_FINER)
!
      end subroutine dealloc_finer_domain_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_work_4_rcb(nnod)
!
      integer(kind = kint), intent(in) :: nnod
!
!      allocate (VAL(nnod))
!      allocate (IS1(nnod))
!
!      VAL = 0.0d0
!      IS1 = 0
!
      end subroutine alloc_work_4_rcb
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_work_4_rcb
!
!      deallocate (VAL)
!      deallocate (IS1)
!
      end subroutine dealloc_work_4_rcb
!
!   --------------------------------------------------------------------
!
      end module t_domain_group_4_partition
