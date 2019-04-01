!
!     module t_data_4_interpolate_org
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine set_area_4_searching                                 &
!!     &         (new_node, new_ele, sph_bin, itp_org_d)
!!      subroutine dealloc_search_area(itp_org_d)
!!        type(node_data), intent(in) :: new_node
!!        type(element_data), intent(in) :: new_ele
!!        type(data_4_interpolate_org), intent(inout) :: itp_org_d
!
      module t_data_4_interpolate_org
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      type data_4_interpolate_org
        integer(kind = kint) :: nele2
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
      end type data_4_interpolate_org
!
      private :: alloc_minmax_4_inter_org
      private :: alloc_nele_in_search_bin, alloc_iele_in_search_bin
      private :: dealloc_minmax_4_inter_org, dealloc_iele_in_search_bin
      private :: check_minmax_sph_org, check_iele_in_search_bin
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_area_4_searching                                   &
     &         (new_node, new_ele, sph_bin, itp_org_d)
!
      use t_geometry_data
      use t_sphere_bin_4_table
!
      use set_minmax_4_each_2nd_ele
      use set_org_ele_4_each_bin
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
      type(sphere_bin_4_table), intent(in) :: sph_bin
!
      type(data_4_interpolate_org), intent(inout) :: itp_org_d
!
!
      call alloc_minmax_4_inter_org(new_ele%numele, itp_org_d)
!
!      write(*,*) 's_set_minmax_sph_each_2nd_ele'
      call s_set_minmax_sph_each_2nd_ele(new_node, new_ele,             &
     &   itp_org_d%nele2, itp_org_d%iflag_meridian_x,                   &
     &   itp_org_d%min_sph_each_ele, itp_org_d%max_sph_each_ele)
!        call check_minmax_sph_org(12, itp_org_d)
!
      call alloc_nele_in_search_bin(sph_bin%ntot_sph_bin, itp_org_d)
!
!      write(*,*) 's_count_num_org_ele_4_each_bin'
      call s_count_num_org_ele_4_each_bin                               &
     &   (new_node, new_ele, sph_bin, itp_org_d%nele2,                  &
     &    itp_org_d%min_sph_each_ele, itp_org_d%max_sph_each_ele,       &
     &    itp_org_d%nele_bin_smp)
!
!      write(*,*) 's_set_bin_stack_4_org_ele'
      call s_set_bin_stack_4_org_ele                                    &
     &   (sph_bin%ntot_sph_bin, itp_org_d%nele_bin_smp,                 &
     &    itp_org_d%nele_bin, itp_org_d%iele_stack_bin,                 &
     &    itp_org_d%iele_stack_bin_smp, itp_org_d%ntot_org_ele_in_bin)
!
!      write(*,*) 'alloc_iele_in_search_bin'
      call alloc_iele_in_search_bin(itp_org_d)
!       write(*,*) 's_set_org_ele_4_each_bin'
      call s_set_org_ele_4_each_bin                                     &
     &   (new_node, new_ele, sph_bin, itp_org_d%nele2,                  &
     &    itp_org_d%min_sph_each_ele, itp_org_d%max_sph_each_ele,       &
     &    itp_org_d%ntot_org_ele_in_bin, itp_org_d%iele_stack_bin_smp,  &
     &    itp_org_d%iele_in_bin, itp_org_d%nele_bin_smp)
!
!
      end subroutine set_area_4_searching
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_search_area(itp_org_d)
!
      type(data_4_interpolate_org), intent(inout) :: itp_org_d
!
      call dealloc_iele_in_search_bin(itp_org_d)
      call dealloc_minmax_4_inter_org(itp_org_d)
!
      end subroutine dealloc_search_area
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_minmax_4_inter_org(nele_2, itp_org_d)
!
      integer(kind = kint), intent(in) :: nele_2
!
      type(data_4_interpolate_org), intent(inout) :: itp_org_d
!
!
      itp_org_d%nele2 = nele_2
      allocate(itp_org_d%iflag_meridian_x(itp_org_d%nele2))
      allocate(itp_org_d%min_sph_each_ele(itp_org_d%nele2,3))
      allocate(itp_org_d%max_sph_each_ele(itp_org_d%nele2,3))
!
      if(itp_org_d%nele2 .le. 0) return
      itp_org_d%iflag_meridian_x = 0
!
      itp_org_d%min_sph_each_ele = 0.0d0
      itp_org_d%max_sph_each_ele = 0.0d0
!
      end subroutine alloc_minmax_4_inter_org
!
!-----------------------------------------------------------------------
!
      subroutine alloc_nele_in_search_bin(ntot_sph_bin, itp_org_d)
!
      integer(kind = kint), intent(in) :: ntot_sph_bin
      type(data_4_interpolate_org), intent(inout) :: itp_org_d
!
      allocate(itp_org_d%nele_bin_smp(np_smp,ntot_sph_bin) )
      allocate(itp_org_d%iele_stack_bin_smp(0:ntot_sph_bin*np_smp) )
      allocate(itp_org_d%nele_bin(ntot_sph_bin) )
      allocate(itp_org_d%iele_stack_bin(0:ntot_sph_bin) )
!
      itp_org_d%nele_bin_smp = 0
      itp_org_d%iele_stack_bin_smp = 0
      itp_org_d%nele_bin = 0
      itp_org_d%iele_stack_bin = 0
!
      end subroutine alloc_nele_in_search_bin
!
!-----------------------------------------------------------------------
!
      subroutine alloc_iele_in_search_bin(itp_org_d)
!
      type(data_4_interpolate_org), intent(inout) :: itp_org_d
!
      allocate(itp_org_d%iele_in_bin(itp_org_d%ntot_org_ele_in_bin))
      itp_org_d%iele_in_bin = 0
!
      end subroutine alloc_iele_in_search_bin
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_minmax_4_inter_org(itp_org_d)
!
      type(data_4_interpolate_org), intent(inout) :: itp_org_d
!
      deallocate(itp_org_d%iflag_meridian_x)
      deallocate(itp_org_d%min_sph_each_ele)
      deallocate(itp_org_d%max_sph_each_ele)
!
      end subroutine dealloc_minmax_4_inter_org
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_iele_in_search_bin(itp_org_d)
!
      type(data_4_interpolate_org), intent(inout) :: itp_org_d
!
      deallocate(itp_org_d%iele_in_bin)
      deallocate(itp_org_d%nele_bin_smp)
      deallocate(itp_org_d%iele_stack_bin_smp)
      deallocate(itp_org_d%nele_bin)
      deallocate(itp_org_d%iele_stack_bin)
!
      end subroutine dealloc_iele_in_search_bin
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_minmax_sph_org(id_file, itp_org_d)
!
      integer(kind = kint), intent(in) :: id_file
      type(data_4_interpolate_org), intent(in) :: itp_org_d
!
      integer(kind = kint) :: iele
!
      write(id_file,*) '#'
      write(id_file,*) '#  min. and max. for radial'
      write(id_file,*) '#'
!
      write(id_file,'(i15)') itp_org_d%nele2
      do iele = 1, itp_org_d%nele2
        write(id_file,'(i15,1p2E25.15e3)') iele,                        &
     &    itp_org_d%min_sph_each_ele(iele,1),                           &
     &    itp_org_d%max_sph_each_ele(iele,1)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  min. and max. for elevation'
      write(id_file,*) '#'
!
      write(id_file,'(i15)') itp_org_d%nele2
      do iele = 1, itp_org_d%nele2
        write(id_file,'(i15,1p2E25.15e3)') iele,                        &
     &      itp_org_d%min_sph_each_ele(iele,2),                         &
     &      itp_org_d%max_sph_each_ele(iele,2)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  min. and max. for azumuth'
      write(id_file,*) '#'
!
      write(id_file,'(i15)') itp_org_d%nele2
      do iele = 1, itp_org_d%nele2
        write(id_file,'(i15,1p2E25.15e3)') iele,                        &
     &      itp_org_d%min_sph_each_ele(iele,3),                         &
     &      itp_org_d%max_sph_each_ele(iele,3)
      end do
!
      end subroutine check_minmax_sph_org
!
!-----------------------------------------------------------------------
!
      subroutine check_iele_in_search_bin                               &
     &         (id_file, ntot_sph_bin, itp_org_d)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ntot_sph_bin
      type(data_4_interpolate_org), intent(in) :: itp_org_d
!
      write(id_file,*) '#'
      write(id_file,*) '#  stack for original elment in search bin'
      write(id_file,*) '#'
!
      write(id_file,'(i15)') ntot_sph_bin
      write(id_file,'(1i16)') itp_org_d%iele_stack_bin(0:ntot_sph_bin)
!
      write(id_file,*) '#'
      write(id_file,*) '#  element for original elment in search bin'
      write(id_file,*) '#'
!
      write(id_file,'(i15)') itp_org_d%ntot_org_ele_in_bin
      write(id_file,'(10i16)')                                          &
     &     itp_org_d%iele_in_bin(1:itp_org_d%ntot_org_ele_in_bin)
!
      end subroutine check_iele_in_search_bin
!
!-----------------------------------------------------------------------
!
      end module t_data_4_interpolate_org
