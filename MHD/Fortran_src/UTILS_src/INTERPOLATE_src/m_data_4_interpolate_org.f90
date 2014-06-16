!
!     module m_data_4_interpolate_org
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_minmax_4_inter_org(nele_2)
!      subroutine allocate_nele_in_search_bin
!      subroutine allocate_iele_in_search_bin
!      subroutine deallocate_minmax_4_inter_org
!      subroutine deallocate_iele_in_search_bin
!
!      subroutine check_minmax_sph_org(id_file, nele_2)
!      subroutine check_iele_in_search_bin(id_file)
!
      module m_data_4_interpolate_org
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: iflag_meridian_x(:)
      real(kind = kreal), allocatable :: min_sph_each_ele(:,:)
      real(kind = kreal), allocatable :: max_sph_each_ele(:,:)
!
      integer(kind = kint) :: ntot_org_ele_in_bin
      integer(kind = kint), allocatable :: nele_bin_smp(:,:)
      integer(kind = kint), allocatable :: nele_bin(:)
      integer(kind = kint), allocatable :: iele_stack_bin_smp(:)
      integer(kind = kint), allocatable :: iele_stack_bin(:)
!
      integer(kind = kint), allocatable :: iele_in_bin(:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_minmax_4_inter_org(nele_2)
!
      integer(kind = kint), intent(in) :: nele_2
!
!
      allocate(iflag_meridian_x(nele_2))
      allocate(min_sph_each_ele(nele_2,3))
      allocate(max_sph_each_ele(nele_2,3))
!
      if(nele_2 .le. 0) return
      iflag_meridian_x = 0
!
      min_sph_each_ele = 0.0d0
      max_sph_each_ele = 0.0d0
!
      end subroutine allocate_minmax_4_inter_org
!
!-----------------------------------------------------------------------
!
      subroutine allocate_nele_in_search_bin
!
      use m_sphere_bin_4_table
      use m_machine_parameter
!
      allocate(nele_bin_smp(np_smp,ntot_sph_bin) )
      allocate(iele_stack_bin_smp(0:ntot_sph_bin*np_smp) )
      allocate(nele_bin(ntot_sph_bin) )
      allocate(iele_stack_bin(0:ntot_sph_bin) )
!
      nele_bin_smp = 0
      iele_stack_bin_smp = 0
      nele_bin = 0
      iele_stack_bin = 0
!
      end subroutine allocate_nele_in_search_bin
!
!-----------------------------------------------------------------------
!
      subroutine allocate_iele_in_search_bin
!
      allocate(iele_in_bin(ntot_org_ele_in_bin))
      iele_in_bin = 0
!
      end subroutine allocate_iele_in_search_bin
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_minmax_4_inter_org
!
      deallocate(iflag_meridian_x)
      deallocate(min_sph_each_ele)
      deallocate(max_sph_each_ele)
!
      end subroutine deallocate_minmax_4_inter_org
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_iele_in_search_bin
!
      deallocate(iele_in_bin)
      deallocate(nele_bin_smp)
      deallocate(iele_stack_bin_smp)
      deallocate(nele_bin)
      deallocate(iele_stack_bin)
!
      end subroutine deallocate_iele_in_search_bin
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_minmax_sph_org(id_file, nele_2)
!
      integer(kind = kint), intent(in) :: nele_2
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: iele
!
      write(id_file,*) '#'
      write(id_file,*) '#  min. and max. for radial'
      write(id_file,*) '#'
!
      write(id_file,'(i10)') nele_2
      do iele = 1, nele_2
        write(id_file,'(i10,1p2e23.12)') iele,                          &
     &      min_sph_each_ele(iele,1), max_sph_each_ele(iele,1)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  min. and max. for elevation'
      write(id_file,*) '#'
!
      write(id_file,'(i10)') nele_2
      do iele = 1, nele_2
        write(id_file,'(i10,1p2e23.12)') iele,                          &
     &      min_sph_each_ele(iele,2), max_sph_each_ele(iele,2)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  min. and max. for azumuth'
      write(id_file,*) '#'
!
      write(id_file,'(i10)') nele_2
      do iele = 1, nele_2
        write(id_file,'(i10,1p2e23.12)') iele,                          &
     &      min_sph_each_ele(iele,3), max_sph_each_ele(iele,3)
      end do
!
      end subroutine check_minmax_sph_org
!
!-----------------------------------------------------------------------
!
      subroutine check_iele_in_search_bin(id_file)
!
      use m_sphere_bin_4_table
!
!
      integer(kind = kint), intent(in) :: id_file
!
      write(id_file,*) '#'
      write(id_file,*) '#  stack for original elment in search bin'
      write(id_file,*) '#'
!
      write(id_file,'(i10)') ntot_sph_bin
      write(id_file,'(10i8)') iele_stack_bin(0:ntot_sph_bin)
!
      write(id_file,*) '#'
      write(id_file,*) '#  element for original elment in search bin'
      write(id_file,*) '#'
!
      write(id_file,'(i10)') ntot_org_ele_in_bin
      write(id_file,'(10i8)') iele_in_bin(1:ntot_org_ele_in_bin)
!
      end subroutine check_iele_in_search_bin
!
!-----------------------------------------------------------------------
!
      end module m_data_4_interpolate_org
