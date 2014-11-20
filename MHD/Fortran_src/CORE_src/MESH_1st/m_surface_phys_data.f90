!m_surface_phys_data.f90
!     module m_surface_phys_data
!.......................................................................
!
!     Written by H. Matsui on Feb., 2008
!
!> @brief Field data on surface for FEM
!
!      subroutine allocate_surf_dat_names
!      subroutine allocate_surf_data_arrays
!      subroutine allocate_surf_fld_id_4_rms
!
!      subroutine deallocate_surf_data_arrays
!      subroutine deallocate_surf_fld_id_4_rms
!
!      subroutine check_surface_phys_data(my_rank, numdir, i_field)
!
!
      module m_surface_phys_data
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint) :: num_surf_phys
      integer (kind=kint) :: ntot_surf_phys
!
      integer (kind=kint), allocatable, target :: num_surf_component(:)
      integer (kind=kint), allocatable, target                          &
     &                    :: istack_surf_component(:)
      integer (kind=kint), allocatable, target :: iorder_surf_phys(:)
      character(len=kchara), allocatable, target :: phys_surf_name(:)
!
      integer (kind=kint), allocatable, target :: iflag_surf_updte(:)
      real(kind=kreal), allocatable, target :: d_surf(:,:)
!
      integer (kind=kint) :: num_surf_phys_vis
      integer (kind=kint) :: num_tot_surf_phys_vis
!
      integer (kind=kint), allocatable, target :: iflag_rms_surf_fld(:)
!
      integer (kind=kint) :: num_surf_phys_4_rms
      integer (kind=kint) :: ntot_comp_surf_phys_4_rms
      integer (kind=kint), allocatable:: ifield_rms_surf(:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_surf_dat_names
!
      allocate( phys_surf_name(num_surf_phys) )
      allocate( num_surf_component(num_surf_phys) )
      allocate( istack_surf_component(0:num_surf_phys) )
      allocate( iorder_surf_phys(num_surf_phys) )
      allocate( iflag_rms_surf_fld(num_surf_phys) )
!
      phys_surf_name = ''
      num_surf_component =    0
      istack_surf_component = 0
      iorder_surf_phys =   1
      iflag_rms_surf_fld = 0
!
      end subroutine allocate_surf_dat_names
!
!  --------------------------------------------------------------------
!
      subroutine allocate_surf_data_arrays
!
      use m_geometry_parameter
!
      allocate( d_surf(numsurf,ntot_surf_phys) )
      allocate( iflag_surf_updte(ntot_surf_phys) )
      d_surf = 0.0d0
      iflag_surf_updte = 0
!
      end subroutine allocate_surf_data_arrays
!
! --------------------------------------------------------------------
!
       subroutine allocate_surf_fld_id_4_rms
!
       allocate (ifield_rms_surf(ntot_comp_surf_phys_4_rms))
       if(ntot_comp_surf_phys_4_rms .gt. 0) ifield_rms_surf = 0
!
       end subroutine allocate_surf_fld_id_4_rms
!
!  --------------------------------------------------------------------
! --------------------------------------------------------------------
!
      subroutine deallocate_surf_data_arrays
!
      deallocate( phys_surf_name )
      deallocate( num_surf_component, istack_surf_component )
      deallocate( iorder_surf_phys, iflag_rms_surf_fld)
      deallocate( d_surf, iflag_surf_updte)
!
      end subroutine deallocate_surf_data_arrays
!
! --------------------------------------------------------------------
!
       subroutine deallocate_surf_fld_id_4_rms
!
       deallocate (ifield_rms_surf)
!
       end subroutine deallocate_surf_fld_id_4_rms
!
!  --------------------------------------------------------------------
! --------------------------------------------------------------------
!
      subroutine check_surface_phys_data(my_rank, numdir, i_field)
!
      use m_geometry_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: numdir, i_field
      integer(kind = kint) :: isurf, nd
!
      write(50+my_rank,*) 'isurf, surface field: ', i_field, numdir
      do isurf = 1, numsurf
        write(50+my_rank,'(i16,1p10e25.14)')                            &
     &         isurf, (d_surf(isurf,i_field+nd-1),nd=1, numdir)
      end do
!
      end subroutine check_surface_phys_data
!
!  --------------------------------------------------------------------
!
      end module m_surface_phys_data
