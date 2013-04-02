!m_work_const_itp_table.f90
!      module m_work_const_itp_table
!
      module m_work_const_itp_table
!
!     Written by H. Matsui on Aug., 2006
!
      use m_precision
!
     implicit none
!
!
      integer(kind = kint), allocatable :: id_search_area(:,:)
      integer(kind = kint), allocatable :: iflag_org_domain(:)
!
      integer(kind = kint), allocatable :: inod_dest_ordered(:)
      integer(kind = kint), allocatable :: iele_orgin_ordered(:)
      real(kind = kreal), allocatable :: coef_inter_ordered(:,:)
      private :: inod_dest_ordered, iele_orgin_ordered
      private :: coef_inter_ordered
!
!      subroutine allocate_work_const_itp_tbl(num_org_pe)
!      subroutine deallocate__work_const_itp_tbl
!
!      subroutine swap_interpolation_table(idest, inod)
!      subroutine copy_table_2_order
!
!      subroutine check_search_ID(id_file)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_work_const_itp_tbl(num_org_pe)
!
      use m_interpolate_table_dest
!
      integer(kind = kint), intent(in) :: num_org_pe
!
      allocate( id_search_area(ntot_table_dest,3) )
      allocate( iflag_org_domain(ntot_table_dest) )
!
      iflag_org_domain = 0
      id_search_area = 0
!
      allocate( inod_dest_ordered(ntot_table_dest) )
      allocate( iele_orgin_ordered(ntot_table_dest) )
      allocate( coef_inter_ordered(ntot_table_dest,3) )
!
     coef_inter_ordered = 0.0d0
     inod_dest_ordered = 0
     iele_orgin_ordered = 0
!
      end subroutine allocate_work_const_itp_tbl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate__work_const_itp_tbl
!
!
      deallocate( id_search_area )
      deallocate( iflag_org_domain )
!
      deallocate( coef_inter_ordered )
      deallocate( iele_orgin_ordered, inod_dest_ordered )
!
      end subroutine deallocate__work_const_itp_tbl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine swap_interpolation_table(idest, inod)
!
      use m_interpolate_table_dest
!
      integer(kind = kint), intent(in) :: idest, inod
!
!
      inod_dest_ordered(idest) =       inod_dest_4_dest(inod)
      iele_orgin_ordered(idest) =      iele_org_4_dest(inod)
      coef_inter_ordered(idest,1:3) = coef_inter_dest(inod,1:3)
!
      end subroutine swap_interpolation_table
!
!-----------------------------------------------------------------------
!
      subroutine copy_table_2_order
!
      use m_interpolate_table_dest
!
!
      inod_dest_4_dest(1:ntot_table_dest)                               &
     &      = inod_dest_ordered(1:ntot_table_dest)
      iele_org_4_dest(1:ntot_table_dest)                                &
     &         = iele_orgin_ordered(1:ntot_table_dest)
!
      coef_inter_dest(1:ntot_table_dest,1)                              &
     &         = coef_inter_ordered(1:ntot_table_dest,1)
      coef_inter_dest(1:ntot_table_dest,2)                              &
     &         = coef_inter_ordered(1:ntot_table_dest,2)
      coef_inter_dest(1:ntot_table_dest,3)                              &
     &         = coef_inter_ordered(1:ntot_table_dest,3)
!
!
      end subroutine copy_table_2_order
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_search_ID(id_file)
!
      use m_interpolate_table_dest
!
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inod
!
      write(id_file,*) '#'
      write(id_file,*) '#  sphere block ID for search'
      write(id_file,*) '#'
!
      write(id_file,'(i10)') ntot_table_dest
      do inod = 1, ntot_table_dest
        write(id_file,'(40i10)') inod, id_search_area(inod,1:3)
      end do
!
      end subroutine check_search_ID
!
!-----------------------------------------------------------------------
!
      subroutine check_table_in_org_1(id_file)
!
      use m_interpolate_table_dest
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inod
!
      write(id_file,*) '#'
      write(id_file,*) '#  node, domain for original, belonged element'
      write(id_file,*) '#   coefficients'
      write(id_file,*) '#'
!
      write(id_file,'(i10)') ntot_table_dest
      do inod = 1, ntot_table_dest
        write(id_file,'(3i10,1p3e23.12)') inod_dest_4_dest(inod),       &
     &        iflag_org_domain(inod), iele_org_4_dest(inod),            &
              coef_inter_dest(inod,1:3)
      end do
!
      end subroutine check_table_in_org_1
!
!-----------------------------------------------------------------------
!
      end module m_work_const_itp_table