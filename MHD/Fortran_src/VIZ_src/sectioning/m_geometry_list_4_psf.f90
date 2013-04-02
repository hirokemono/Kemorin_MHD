!
!      module m_geometry_list_4_psf
!
      module m_geometry_list_4_psf
!
!      Written by H. Matsui on June, 2006
!
      use m_precision
!
      implicit none
!
!
      real(kind = kreal), allocatable :: c_ref_psf(:,:)
      integer(kind = kint), allocatable :: mark_ele_psf(:)
!
!
      integer(kind = kint) :: nnod_on_nod_psf_tot
      integer(kind = kint), allocatable :: istack_n_on_n_psf_smp(:)
!
      integer(kind = kint), allocatable :: inod_4_nod_psf(:)
      integer(kind = kint), allocatable :: iflag_n_on_n_psf(:,:)
      integer(kind = kint), allocatable :: id_n_on_n_psf(:,:)
!
      real(kind = kreal), allocatable :: coef_on_nod_psf(:)
!
!
      integer(kind = kint) :: nnod_on_edge_psf_tot
      integer(kind = kint), allocatable :: istack_n_on_e_psf_smp(:)
!
      integer(kind = kint), allocatable :: iedge_4_nod_psf(:)
      integer(kind = kint), allocatable :: iflag_n_on_e_psf(:,:)
      integer(kind = kint), allocatable :: id_n_on_e_psf(:,:)
!
      real(kind = kreal), allocatable :: coef_on_edge_psf(:,:)
!
!
      real(kind = kreal), allocatable :: data_on_nod_psf(:,:)
      real(kind = kreal), allocatable :: data_on_edge_psf(:,:)
!
      real(kind = kreal), allocatable :: tmp_on_nod_psf(:,:)
      real(kind = kreal), allocatable :: tmp_on_edge_psf(:,:)
!
!      subroutine allocate_constant_4_ref_psf(num_psf, numnod, nele_s)
!
!      subroutine allocate_nnod_psf(np_smp, num_psf, numnod, numedge)
!      subroutine allocate_inod_psf(num_psf)
!
!      subroutine deallocate_constant_4_ref_psf
!      subroutine deallocate_nnod_psf
!      subroutine deallocate_inod_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_constant_4_ref_psf(num_psf, numnod, nele_s)
!
      integer(kind= kint), intent(in) :: num_psf, numnod, nele_s
!
!
      allocate( c_ref_psf(numnod,num_psf) )
      allocate( mark_ele_psf(nele_s))
      c_ref_psf = 0.0d0
      mark_ele_psf = 0
!
      end subroutine allocate_constant_4_ref_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_nnod_psf(np_smp, num_psf, numnod, numedge)
!
      integer(kind= kint), intent(in) :: np_smp, num_psf
      integer(kind= kint), intent(in) :: numnod, numedge
!
      allocate(istack_n_on_n_psf_smp(0:np_smp*num_psf))
      allocate(istack_n_on_e_psf_smp(0:np_smp*num_psf))
!
      allocate(iflag_n_on_n_psf(numnod,num_psf))
      allocate(id_n_on_n_psf(numnod,num_psf))
      allocate(iflag_n_on_e_psf(numedge,num_psf))
      allocate(id_n_on_e_psf(numedge,num_psf))
!
      istack_n_on_n_psf_smp = 0
      istack_n_on_e_psf_smp = 0
!
      iflag_n_on_n_psf = 0
      id_n_on_n_psf = 0
      iflag_n_on_e_psf = 0
      id_n_on_e_psf = 0
!
      end subroutine allocate_nnod_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_inod_psf(num_psf)
!
      integer(kind= kint), intent(in) :: num_psf
!
      allocate(inod_4_nod_psf(nnod_on_nod_psf_tot))
      allocate(coef_on_nod_psf(nnod_on_nod_psf_tot))
!
      allocate(iedge_4_nod_psf(nnod_on_edge_psf_tot))
      allocate(coef_on_edge_psf(nnod_on_edge_psf_tot,2))
!
      inod_4_nod_psf = 0
      coef_on_nod_psf = 0.0d0
!
      iedge_4_nod_psf = 0
      coef_on_edge_psf = 0.0d0
!
      end subroutine allocate_inod_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_constant_4_ref_psf
!
      deallocate( c_ref_psf )
      deallocate( mark_ele_psf)
!
      end subroutine deallocate_constant_4_ref_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_nnod_psf
!
      deallocate(istack_n_on_n_psf_smp)
      deallocate(istack_n_on_e_psf_smp)
!
      deallocate(iflag_n_on_n_psf)
      deallocate(id_n_on_n_psf)
      deallocate(iflag_n_on_e_psf)
      deallocate(id_n_on_e_psf)
!
      end subroutine deallocate_nnod_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_inod_psf
!
      deallocate(inod_4_nod_psf)
      deallocate(coef_on_nod_psf)
!
      deallocate(iedge_4_nod_psf)
      deallocate(coef_on_edge_psf)
!
      end subroutine deallocate_inod_psf
!
!  ---------------------------------------------------------------------
!
      end module m_geometry_list_4_psf
