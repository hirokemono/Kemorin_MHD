!m_element_phys_data.f90
!     module m_element_phys_data
!.......................................................................
!
!     Written by H. Matsui on Nov., 2011
!
!> @brief Field data in element for FEM
!
!      subroutine allocate_ele_data_arrays(numele)
!
!      subroutine deallocate_ele_data_arrays
!
!      subroutine check_elemental_data(my_rank, numdir, i_field)
!
!
      module m_element_phys_data
!
      use m_precision
      use t_phys_data
!
      implicit  none
!
!
!>       Structure for field data on element
      type(phys_data), save :: fld_ele1
!fld_ele1%iflag_monitor
!
!      integer (kind=kint) :: num_ele_phys
!      integer (kind=kint) :: num_tot_ele_phys
!
!      integer (kind=kint), allocatable, target :: num_ele_component(:)
!      integer (kind=kint), allocatable, target                         &
!     &                    :: istack_ele_component(:)
!      integer (kind=kint), allocatable, target :: iorder_ele_phys(:)
!      character (len=kchara), allocatable, target :: phys_ele_name(:)
!
!      integer (kind=kint), allocatable, target :: iflag_ele_update(:)
      real (kind=kreal), allocatable, target :: d_ele(:,:)
!
!      integer (kind=kint) :: num_ele_phys_vis
!      integer (kind=kint) :: num_tot_ele_phys_vis
!
!      integer (kind=kint), allocatable, target :: iflag_rms_ele_fld(:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_ele_data_arrays(numele)
!
      integer(kind = kint), intent(in) :: numele
!
      allocate( fld_ele1%iflag_update(fld_ele1%ntot_phys) )
      allocate( d_ele(numele,fld_ele1%ntot_phys) )
!
      fld_ele1%iflag_update = 0
      d_ele = 0.0d0
!
       end subroutine allocate_ele_data_arrays
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine deallocate_ele_data_arrays
!
!
      call dealloc_phys_name_type(fld_ele1)
      deallocate( fld_ele1%iflag_update )
      deallocate( d_ele )
!
      end subroutine deallocate_ele_data_arrays
!
!  --------------------------------------------------------------------
! --------------------------------------------------------------------
!
      subroutine check_elemental_data(my_rank, numdir, i_field)
!
       use m_geometry_data
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: numdir, i_field
      integer(kind = kint) :: iele, nd
!
      write(50+my_rank,*) 'iele, elemental field: ', i_field, numdir
      do iele = 1, ele1%numele
        write(50+my_rank,'(i16,1p10e25.14)')                            &
     &         iele, (d_ele(iele,i_field+nd-1),nd=1, numdir)
      end do
!
      end subroutine check_elemental_data
!
!  --------------------------------------------------------------------
!
      end module m_element_phys_data
