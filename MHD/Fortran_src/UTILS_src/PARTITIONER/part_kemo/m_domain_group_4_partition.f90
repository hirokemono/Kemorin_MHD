!m_domain_group_4_partition.f90
!     module m_domain_group_4_partition
!
!      Written by H. Matsui on Aug., 2007
!
!      subroutine allocate_domain_nod_group
!      subroutine allocate_domain_nese_group
!      subroutine allocate_local_ne_id_tbl
!      subroutine allocate_local_nese_id_tbl
!      subroutine allocate_finer_domain_group
!
!      subroutine deallocate_domain_nod_group
!      subroutine deallocate_domain_nese_group
!      subroutine deallocate_local_nese_id_tbl
!      subroutine deallocate_finer_domain_group
!
!      subroutine allocate_work_4_rcb(nnod)
!      subroutine deallocate_work_4_rcb
!
      module m_domain_group_4_partition
!
      use m_precision
      use t_domain_group_4_partition
!
      implicit none
!
      integer(kind = kint) :: intnod_s_domin
!
      integer(kind = kint), allocatable :: IGROUP_nod(:)
      integer(kind = kint), allocatable :: IGROUP_ele(:)
      integer(kind = kint), allocatable :: IGROUP_surf(:)
      integer(kind = kint), allocatable :: IGROUP_edge(:)
!
      integer(kind = kint), allocatable :: inod_local_part(:)
      integer(kind = kint), allocatable :: iele_local_part(:)
      integer(kind = kint), allocatable :: isurf_local_part(:)
!
!
      integer(kind = kint) :: nproc_finer
      integer(kind = kint) :: nnod_group_finer, internod_group_finer
      integer(kind = kint),  allocatable :: IGROUP_FINER(:)
!
      real(kind=kreal), allocatable :: VAL(:)
      integer(kind=kint), allocatable :: IS1(:)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_domain_nod_group
!
      allocate(IGROUP_nod(nod_d_grp1%num_s_domin))
      allocate(IGROUP_ele(ele_d_grp1%num_s_domin))
      IGROUP_nod = 0
      IGROUP_ele = 0
!
      end subroutine allocate_domain_nod_group
!
!   --------------------------------------------------------------------
!
      subroutine allocate_domain_nese_group
!
      allocate(IGROUP_nod(nod_d_grp1%num_s_domin))
      allocate(IGROUP_ele(ele_d_grp1%num_s_domin))
      allocate(IGROUP_edge(edge_d_grp1%num_s_domin))
      allocate(IGROUP_surf(surf_d_grp1%num_s_domin))
      IGROUP_nod = 0
      IGROUP_ele = 0
      IGROUP_edge = 0
      IGROUP_surf = 0
!
      end subroutine allocate_domain_nese_group
!
!   --------------------------------------------------------------------
!
      subroutine allocate_local_ne_id_tbl
!
      allocate(inod_local_part(nod_d_grp1%num_s_domin))
      allocate(iele_local_part(ele_d_grp1%num_s_domin))
      inod_local_part =  0
      iele_local_part =  0
!
      end subroutine allocate_local_ne_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine allocate_local_nese_id_tbl
!
      allocate(inod_local_part(nod_d_grp1%num_s_domin))
      allocate(iele_local_part(ele_d_grp1%num_s_domin))
      allocate(isurf_local_part(surf_d_grp1%num_s_domin))
      allocate(edge_d_grp1%id_local_part(edge_d_grp1%num_s_domin))
      inod_local_part =  0
      iele_local_part =  0
      edge_d_grp1%id_local_part = 0
      isurf_local_part = 0
!
      end subroutine allocate_local_nese_id_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine allocate_finer_domain_group
!
      allocate(IGROUP_FINER(nnod_group_finer))
      IGROUP_FINER = 0
!
      end subroutine allocate_finer_domain_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine deallocate_domain_nod_group
!
      deallocate(IGROUP_nod, IGROUP_ele)
!
      end subroutine deallocate_domain_nod_group
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_domain_nese_group
!
      deallocate(IGROUP_nod, IGROUP_ele)
      deallocate(IGROUP_surf, IGROUP_edge)
!
      end subroutine deallocate_domain_nese_group
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_local_ne_id_tbl
!
!
      deallocate(inod_local_part, iele_local_part)
!
      end subroutine deallocate_local_ne_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_local_nese_id_tbl
!
      deallocate(inod_local_part, iele_local_part)
      deallocate(isurf_local_part, edge_d_grp1%id_local_part)
!
      end subroutine deallocate_local_nese_id_tbl
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_finer_domain_group
!
      deallocate(IGROUP_FINER)
!
      end subroutine deallocate_finer_domain_group
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine allocate_work_4_rcb(nnod)
!
      integer(kind = kint), intent(in) :: nnod
!
      allocate (VAL(nnod))
      allocate (IS1(nnod))
!
      VAL = 0.0d0
      IS1 = 0
!
      end subroutine allocate_work_4_rcb
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_work_4_rcb
!
      deallocate (VAL)
      deallocate (IS1)
!
      end subroutine deallocate_work_4_rcb
!
!   --------------------------------------------------------------------
!
      end module m_domain_group_4_partition
