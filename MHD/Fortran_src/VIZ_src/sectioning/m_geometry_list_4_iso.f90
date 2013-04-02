!
!      module m_geometry_list_4_iso
!
      module m_geometry_list_4_iso
!
!      Written by H. Matsui on June, 2006
!
      use m_precision
!
      implicit none
!
!
      real(kind = kreal), allocatable :: c_ref_iso(:,:)
      integer(kind = kint), allocatable :: mark_ele_iso(:)
!
!
      integer(kind = kint) :: nnod_on_nod_iso_tot
      integer(kind = kint), allocatable :: istack_n_on_n_iso_smp(:)
!
      integer(kind = kint), allocatable :: inod_4_nod_iso(:)
      integer(kind = kint), allocatable :: iflag_n_on_n_iso(:,:)
      integer(kind = kint), allocatable :: id_n_on_n_iso(:,:)
!
      real(kind = kreal), allocatable :: coef_on_nod_iso(:)
!
!
      integer(kind = kint) :: nnod_on_edge_iso_tot
      integer(kind = kint), allocatable :: istack_n_on_e_iso_smp(:)
!
      integer(kind = kint), allocatable :: iedge_4_nod_iso(:)
      integer(kind = kint), allocatable :: iflag_n_on_e_iso(:,:)
      integer(kind = kint), allocatable :: id_n_on_e_iso(:,:)
!
      real(kind = kreal), allocatable :: coef_on_edge_iso(:,:)
!
!
      real(kind = kreal), allocatable :: data_on_nod_iso(:,:)
      real(kind = kreal), allocatable :: data_on_edge_iso(:,:)
!
      real(kind = kreal), allocatable :: tmp_on_nod_iso(:,:)
      real(kind = kreal), allocatable :: tmp_on_edge_iso(:,:)
!
!      subroutine allocate_constant_4_ref_iso(num_iso, numnod, nele_s)
!
!      subroutine allocate_nnod_iso(np_smp, num_iso, numnod, numedge)
!      subroutine allocate_inod_iso(num_iso)
!
!      subroutine deallocate_constant_4_ref_iso
!      subroutine deallocate_nnod_iso
!      subroutine deallocate_inod_iso
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_constant_4_ref_iso(num_iso, numnod, nele_s)
!
      integer(kind= kint), intent(in) :: num_iso, numnod, nele_s
!
!
      allocate( c_ref_iso(numnod,num_iso) )
      allocate( mark_ele_iso(nele_s))
      c_ref_iso = 0.0d0
      mark_ele_iso = 0
!
      end subroutine allocate_constant_4_ref_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_nnod_iso(np_smp, num_iso, numnod, numedge)
!
      integer(kind= kint), intent(in) :: np_smp, num_iso
      integer(kind= kint), intent(in) :: numnod, numedge
!
      allocate(istack_n_on_n_iso_smp(0:np_smp*num_iso))
      allocate(istack_n_on_e_iso_smp(0:np_smp*num_iso))
      allocate(iflag_n_on_n_iso(numnod,num_iso))
      allocate(id_n_on_n_iso(numnod,num_iso))
      allocate(iflag_n_on_e_iso(numedge,num_iso))
      allocate(id_n_on_e_iso(numedge,num_iso))
!
      istack_n_on_n_iso_smp = 0
      istack_n_on_e_iso_smp = 0
      iflag_n_on_n_iso = 0
      id_n_on_n_iso = 0
      iflag_n_on_e_iso = 0
      id_n_on_e_iso = 0
!
      end subroutine allocate_nnod_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_inod_iso(num_iso)
!
      integer(kind= kint), intent(in) :: num_iso
!
      allocate(inod_4_nod_iso(nnod_on_nod_iso_tot))
      allocate(coef_on_nod_iso(nnod_on_nod_iso_tot))
!
      allocate(iedge_4_nod_iso(nnod_on_edge_iso_tot))
      allocate(coef_on_edge_iso(nnod_on_edge_iso_tot,2))
!
      inod_4_nod_iso = 0
      coef_on_nod_iso = 0.0d0
!
      iedge_4_nod_iso = 0
      coef_on_edge_iso = 0.0d0
!
      end subroutine allocate_inod_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_constant_4_ref_iso
!
      deallocate( c_ref_iso )
      deallocate( mark_ele_iso)
!
      end subroutine deallocate_constant_4_ref_iso
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_nnod_iso
!
      deallocate(istack_n_on_n_iso_smp)
      deallocate(istack_n_on_e_iso_smp)
      deallocate(iflag_n_on_n_iso)
      deallocate(id_n_on_n_iso)
      deallocate(iflag_n_on_e_iso)
      deallocate(id_n_on_e_iso)
!
      end subroutine deallocate_nnod_iso
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_inod_iso
!
      deallocate(inod_4_nod_iso)
      deallocate(coef_on_nod_iso)
      deallocate(iedge_4_nod_iso)
      deallocate(coef_on_edge_iso)
!
      end subroutine deallocate_inod_iso
!
!  ---------------------------------------------------------------------
!
      end module m_geometry_list_4_iso
