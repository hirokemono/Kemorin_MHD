!m_element_phys_data.f90
!     module m_element_phys_data
!.......................................................................
!
!     Written by H. Matsui on Nov., 2011
!
!> @brief Field data in element for FEM
!
!      subroutine allocate_ele_dat_names
!      subroutine allocate_ele_data_arrays(numele)
!      subroutine allocate_ele_fld_id_4_rms
!
!      subroutine deallocate_ele_data_arrays
!      subroutine deallocate_ele_fld_id_4_rms
!
!      subroutine check_elemental_data(my_rank, numdir, i_field)
!
!
      module m_element_phys_data
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint) :: num_ele_phys
      integer (kind=kint) :: num_tot_ele_phys
!
      integer (kind=kint), allocatable, target :: num_ele_component(:)
      integer (kind=kint), allocatable, target                          &
     &                    :: istack_ele_component(:)
      integer (kind=kint), allocatable, target :: iorder_ele_phys(:)
      character (len=kchara), allocatable, target :: phys_ele_name(:)
!
      integer (kind=kint), allocatable, target :: iflag_ele_update(:)
      real (kind=kreal), allocatable, target :: d_ele(:,:)
!
      integer (kind=kint) :: num_ele_phys_vis
      integer (kind=kint) :: num_tot_ele_phys_vis
!
      integer (kind=kint), allocatable, target :: iflag_rms_ele_fld(:)
!
      integer (kind=kint) :: num_ele_phys_4_rms
      integer (kind=kint) :: ntot_comp_ele_phys_4_rms
      integer (kind=kint), allocatable:: ifield_rms_ele(:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
       subroutine allocate_ele_dat_names
!
          allocate( phys_ele_name(num_ele_phys) )
          allocate( num_ele_component(num_ele_phys) )
          allocate( istack_ele_component(0:num_ele_phys) )
          allocate( iorder_ele_phys(num_ele_phys) )
          allocate( iflag_rms_ele_fld(num_ele_phys) )
!
          phys_ele_name = ''
          num_ele_component =    0
          istack_ele_component = 0
          iorder_ele_phys =      1
          iflag_rms_ele_fld =    0
!
       end subroutine allocate_ele_dat_names
!
!  --------------------------------------------------------------------
!
      subroutine allocate_ele_data_arrays(numele)
!
      integer(kind = kint), intent(in) :: numele
!
      allocate( iflag_ele_update(num_tot_ele_phys) )
      allocate( d_ele(numele,num_tot_ele_phys) )
!
      iflag_ele_update = 0
      d_ele = 0.0d0
!
       end subroutine allocate_ele_data_arrays
!
!  --------------------------------------------------------------------
!
       subroutine allocate_ele_fld_id_4_rms
!
       allocate (ifield_rms_ele(ntot_comp_ele_phys_4_rms))
       if(ntot_comp_ele_phys_4_rms .gt. 0) ifield_rms_ele = 0
!
       end subroutine allocate_ele_fld_id_4_rms
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine deallocate_ele_data_arrays
!
!
      deallocate( phys_ele_name )
      deallocate( num_ele_component, istack_ele_component )
      deallocate( iorder_ele_phys, iflag_rms_ele_fld )
      deallocate( iflag_ele_update )
      deallocate( d_ele )
!
      end subroutine deallocate_ele_data_arrays
!
!  --------------------------------------------------------------------
!
       subroutine deallocate_ele_fld_id_4_rms
!
       deallocate (ifield_rms_ele)
!
       end subroutine deallocate_ele_fld_id_4_rms
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
