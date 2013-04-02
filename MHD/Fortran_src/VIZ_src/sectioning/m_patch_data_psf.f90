!
!      module m_patch_data_psf
!
      module m_patch_data_psf
!
!      Written by H. Matsui on June, 2006
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: nnod_psf_tot
      integer(kind = kint), allocatable :: istack_nod_psf_smp(:)
!
      integer(kind = kint), allocatable :: inod_hash_psf(:)
      real(kind = kreal), allocatable :: xyz_psf(:,:)
!
      real(kind = kreal), allocatable :: sph_psf(:,:)
      real(kind = kreal), allocatable :: cyl_psf(:,:)
!
!
      integer(kind = kint) :: npatch_tot_psf_smp
      integer(kind = kint), allocatable :: istack_patch_psf_smp(:)
      integer(kind = kint), allocatable :: ie_patch_psf(:,:)
!
!
      real(kind = kreal), allocatable :: dat_psf(:,:)
!
      real(kind = kreal), allocatable :: tmp_psf(:,:)
!
!      subroutine allocate_position_psf(num_psf)
!      subroutine allocate_dat_on_patch_psf(max_ncomp_psf)
!      subroutine allocate_num_patch_psf(np_smp, num_psf)
!      subroutine allocate_patch_data_psf
!
!      subroutine deallocate_position_psf
!      subroutine deallocate_dat_on_patch_psf
!      subroutine deallocate_num_patch_psf
!      subroutine deallocate_patch_data_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_position_psf(num_psf)
!
      integer(kind= kint), intent(in) :: num_psf
!
      allocate(inod_hash_psf(nnod_psf_tot))
      allocate(xyz_psf(nnod_psf_tot,3))
      allocate(sph_psf(nnod_psf_tot,4))
      allocate(cyl_psf(nnod_psf_tot,2))
!
      if(nnod_psf_tot .gt. 0) then
        inod_hash_psf = 0
        xyz_psf = 0.0d0
        sph_psf = 0.0d0
        cyl_psf = 0.0d0
      end if
!
      end subroutine allocate_position_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_dat_on_patch_psf(max_ncomp_psf)
!
      integer(kind = kint), intent(in) :: max_ncomp_psf
!
      allocate(dat_psf(nnod_psf_tot,max_ncomp_psf))
      allocate(tmp_psf(nnod_psf_tot,6))
!
      if(nnod_psf_tot .gt. 0) then
        dat_psf = 0.0d0
        tmp_psf = 0.0d0
      end if
!
      end subroutine allocate_dat_on_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_patch_psf(np_smp, num_psf)
!
      integer(kind= kint), intent(in) :: np_smp, num_psf
!
      allocate(istack_nod_psf_smp(0:np_smp*num_psf))
      allocate(istack_patch_psf_smp(0:np_smp*num_psf))
!
      if(num_psf .gt. 0) then
        istack_nod_psf_smp = 0
        istack_patch_psf_smp = 0
      end if
!
      end subroutine allocate_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_patch_data_psf
!
      allocate(ie_patch_psf(npatch_tot_psf_smp,3))
      if(npatch_tot_psf_smp .gt. 0) ie_patch_psf = 0
!
      end subroutine allocate_patch_data_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_position_psf
!
      deallocate(xyz_psf)
      deallocate(sph_psf)
      deallocate(cyl_psf)
!
      end subroutine deallocate_position_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_dat_on_patch_psf
!
      deallocate(dat_psf)
      deallocate(tmp_psf)
!
      end subroutine deallocate_dat_on_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_patch_psf
!
      deallocate(istack_nod_psf_smp)
      deallocate(istack_patch_psf_smp)
!
      end subroutine deallocate_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_patch_data_psf
!
      deallocate(ie_patch_psf)
!
      end subroutine deallocate_patch_data_psf
!
!  ---------------------------------------------------------------------
!
      end module m_patch_data_psf
