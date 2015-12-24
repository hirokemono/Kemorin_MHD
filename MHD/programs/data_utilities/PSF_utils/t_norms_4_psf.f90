!
!      module t_norms_4_psf
!
!      Written by H. Matsui
!
!      subroutine allocate_norms_4_psf(psf_nod, psf_ele, psf_norm)
!      subroutine alloc_psf_averages(psf_phys, psf_aves)
!      subroutine deallocate_norms_4_psf(psf_norm)
!      subroutine dealloc_psf_averages(psf_aves)
!
!      subroutine check_center_ele_psf(psf_norm)
!      subroutine check_ele_normal_psf(psf_norm)
!      subroutine check_nod_normal_psf(psf_norm)
!
      module t_norms_4_psf
!
      use m_precision
!
      implicit none
!
      type psf_normals
        integer(kind = kint) :: nele
        integer(kind = kint) :: nnod
!
        real(kind = kreal), pointer :: center_ele(:,:)
!
        real(kind = kreal), pointer :: r_ele(:)
        real(kind = kreal), pointer :: rflag_ave(:)
!
        real(kind = kreal), pointer :: norm_ele(:,:)
        real(kind = kreal), pointer :: norm_nod(:,:)
        real(kind = kreal), pointer :: area_ele(:)
        real(kind = kreal), pointer :: weight_4_nod(:)
!
        real(kind = kreal) :: area
      end type psf_normals
!
      type psf_averages
        integer(kind = kint) :: ntot_comp
        real(kind = kreal), allocatable :: ave(:)
        real(kind = kreal), allocatable :: rms(:)
        real(kind = kreal), allocatable :: sdev(:)
!
        real(kind = kreal), allocatable :: dmin(:)
        real(kind = kreal), allocatable :: dmax(:)
      end type psf_averages
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_norms_4_psf(psf_nod, psf_ele, psf_norm)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      type(psf_normals), intent(inout) :: psf_norm
!
!
      psf_norm%nele = psf_ele%numele
      allocate ( psf_norm%center_ele(psf_norm%nele,3) )
      allocate ( psf_norm%r_ele(psf_norm%nele) )
      allocate ( psf_norm%rflag_ave(psf_norm%nele) )
!
      allocate ( psf_norm%norm_ele(psf_norm%nele,3) )
      allocate ( psf_norm%area_ele(psf_norm%nele) )
!
      psf_norm%nnod = psf_nod%numnod
      allocate ( psf_norm%weight_4_nod(psf_norm%nnod) )
      allocate ( psf_norm%norm_nod(psf_norm%nnod,3) )
!
      psf_norm%center_ele = 0.0d0
      psf_norm%r_ele = 0.0d0
      psf_norm%rflag_ave = 0.0d0
      psf_norm%norm_ele = 0.0d0
      psf_norm%area_ele = 0.0d0
      psf_norm%weight_4_nod = 0.0d0
      psf_norm%norm_nod = 0.0d0
!
      end subroutine allocate_norms_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine alloc_psf_averages(psf_phys, psf_aves)
!
      use t_phys_data
!
      type(phys_data), intent(in) :: psf_phys
      type(psf_averages), intent(inout) :: psf_aves
!
!
      psf_aves%ntot_comp = psf_phys%ntot_phys
      allocate ( psf_aves%dmin(psf_phys%ntot_phys) )
      allocate ( psf_aves%dmax(psf_phys%ntot_phys) )
      allocate ( psf_aves%ave(psf_phys%ntot_phys) )
      allocate ( psf_aves%rms(psf_phys%ntot_phys) )
      allocate ( psf_aves%sdev(psf_phys%ntot_phys) )
!
      psf_aves%dmin = 1.0d30
      psf_aves%dmax = 0.0d0
      psf_aves%ave =  0.0d0
      psf_aves%rms =  0.0d0
      psf_aves%sdev = 0.0d0
!
      end subroutine alloc_psf_averages
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_norms_4_psf(psf_norm)
!
      type(psf_normals), intent(inout) :: psf_norm
!
!
      deallocate ( psf_norm%center_ele, psf_norm%r_ele )
      deallocate ( psf_norm%norm_ele, psf_norm%area_ele )
      deallocate ( psf_norm%rflag_ave, psf_norm%weight_4_nod )
      deallocate ( psf_norm%norm_nod )
!
      end subroutine deallocate_norms_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_psf_averages(psf_aves)
!
      type(psf_averages), intent(inout) :: psf_aves
!
      deallocate ( psf_aves%dmin )
      deallocate ( psf_aves%dmax )
      deallocate ( psf_aves%ave, psf_aves%rms, psf_aves%sdev )
!
      end subroutine dealloc_psf_averages
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_center_ele_psf(psf_norm)
!
      type(psf_normals), intent(inout) :: psf_norm
      integer(kind = kint) :: iele
!
      write(50,*)                                                       &
     &     'iele, radius_ele_psf(iele), psf_norm%center_ele(iele,1:3)'
      do iele = 1, psf_norm%nele
        write(50,*) iele, psf_norm%r_ele(iele),                        &
     &               psf_norm%center_ele(iele,1:3)
      end do
!
      end subroutine check_center_ele_psf
!
!-----------------------------------------------------------------------
!
      subroutine check_ele_normal_psf(psf_norm)
!
      type(psf_normals), intent(inout) :: psf_norm
      integer(kind = kint) :: iele
!
      write(50,*) 'iele, area_psf(iele), psf_norm%norm_ele(iele,1:3)'
      do iele = 1, psf_norm%nele
        write(50,*) iele, psf_norm%area_ele(iele),                     &
     &              psf_norm%norm_ele(iele,1:3)
      end do
!
      end subroutine check_ele_normal_psf
!
!-----------------------------------------------------------------------
!
      subroutine check_nod_normal_psf(psf_norm)
!
      type(psf_normals), intent(inout) :: psf_norm
      integer(kind = kint) :: inod
!
      write(50,*) 'inod, norm_nod_psf(inod,1:3)'
      do inod = 1, psf_norm%nnod
        write(50,*) inod, psf_norm%norm_nod(inod,1:3)
      end do
!
      end subroutine check_nod_normal_psf
!
!-----------------------------------------------------------------------
!
      end module  t_norms_4_psf
