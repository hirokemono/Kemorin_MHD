!>@file   m_patch_data_iso.f90
!!        module m_patch_data_iso
!!
!! @author H. Matsui
!! @date   Programmed in ?June, 2006??
!!
!
!> @brief data array for isosurfaces
!!
!!@verbatim
!!      subroutine allocate_position_iso
!!      subroutine allocate_dat_on_patch_iso(max_ncomp_iso)
!!      subroutine allocate_num_patch_iso(np_smp, num_iso)
!!      subroutine allocate_patch_data_iso
!!
!!      subroutine deallocate_position_iso
!!      subroutine deallocate_dat_on_patch_iso
!!      subroutine deallocate_num_patch_iso
!!      subroutine deallocate_patch_data_iso
!!@endverbatim
!
      module m_patch_data_iso
!
      use m_precision
!
      implicit none
!
!
!>      Total number of nodes for isosurfaces
      integer(kind = kint) :: nnod_iso_tot
!>      End point of node list for each isosurfaces
      integer(kind = kint), allocatable :: istack_nod_iso_smp(:)
!
      integer(kind = kint), allocatable :: inod_hash_iso(:)
      real(kind = kreal), allocatable :: xyz_iso(:,:)
!
      real(kind = kreal), allocatable :: sph_iso(:,:)
      real(kind = kreal), allocatable :: cyl_iso(:,:)
!
!
      integer(kind = kint) :: npatch_tot_iso_smp
      integer(kind = kint), allocatable :: istack_patch_iso_smp(:)
      integer(kind = kint), allocatable :: ie_patch_iso(:,:)
!
!
      real(kind = kreal), allocatable :: dat_iso(:,:)
!
      real(kind = kreal), allocatable :: tmp_iso(:,:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_position_iso
!
      allocate(inod_hash_iso(nnod_iso_tot))
      allocate(xyz_iso(nnod_iso_tot,3))
      allocate(sph_iso(nnod_iso_tot,4))
      allocate(cyl_iso(nnod_iso_tot,2))
!
      if(nnod_iso_tot .gt. 0) then
        inod_hash_iso = 0
        xyz_iso = 0.0d0
        sph_iso = 0.0d0
        cyl_iso = 0.0d0
      end if
!
      end subroutine allocate_position_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_dat_on_patch_iso(max_ncomp_iso)
!
      integer(kind = kint), intent(in) :: max_ncomp_iso
!
      allocate(dat_iso(nnod_iso_tot,max_ncomp_iso))
      allocate(tmp_iso(nnod_iso_tot,6))
!
      if(nnod_iso_tot .gt. 0) then
        dat_iso = 0.0d0
        tmp_iso = 0.0d0
      end if
!
      end subroutine allocate_dat_on_patch_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_patch_iso(np_smp, num_iso)
!
      integer(kind= kint), intent(in) :: np_smp, num_iso
!
      allocate(istack_nod_iso_smp(0:np_smp*num_iso))
      allocate(istack_patch_iso_smp(0:np_smp*num_iso))
!
      if(num_iso .gt. 0) then
        istack_nod_iso_smp = 0
        istack_patch_iso_smp = 0
      end if
!
      end subroutine allocate_num_patch_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_patch_data_iso
!
      allocate(ie_patch_iso(npatch_tot_iso_smp,3))
      if(npatch_tot_iso_smp .gt. 0) ie_patch_iso = 0
!
      end subroutine allocate_patch_data_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_position_iso
!
      deallocate(inod_hash_iso)
      deallocate(xyz_iso)
      deallocate(sph_iso)
      deallocate(cyl_iso)
!
      end subroutine deallocate_position_iso
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_dat_on_patch_iso
!
      deallocate(dat_iso)
      deallocate(tmp_iso)
!
      end subroutine deallocate_dat_on_patch_iso
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_patch_iso
!
      deallocate(istack_nod_iso_smp)
      deallocate(istack_patch_iso_smp)
!
      end subroutine deallocate_num_patch_iso
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_patch_data_iso
!
      deallocate(ie_patch_iso)
!
      end subroutine deallocate_patch_data_iso
!
!  ---------------------------------------------------------------------

      end module m_patch_data_iso
