!>@file   m_psf_results.f90
!!@brief  module m_psf_results
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief set edge information for PSF results
!!
!!@verbatim
!!      subroutine allocate_psf_results
!!      subroutine allocate_psf_num_field
!!      subroutine allocate_psf_field_data
!!
!!      subroutine deallocate_psf_results
!!
!!      subroutine count_stack_tot_psf_field
!!
!!      subroutine set_psf_mesh_to_ucd_data(psf_ucd)
!!      subroutine set_psf_mesh_to_ucd_mesh(psf_ucd)
!!      subroutine set_psf_mesh_to_ucd_field(psf_ucd)
!!        type(ucd_data), intent(inout) :: psf_ucd
!!@endverbatim
!
      module m_psf_results
!
      use m_precision
      use m_field_file_format
!
      implicit none
!
      character(len=kchara) :: psf_file_header
      integer(kind = kint) :: iflag_psf_fmt = iflag_udt
!
      integer(kind = kint) :: numnod_psf, numele_psf, nnod_4_ele_psf
      integer(kind = kint) :: nfield_psf, ncomptot_psf
!
      real(kind = kreal), allocatable, target :: xx_psf(:,:)
      real(kind = kreal), allocatable, target :: d_nod_psf(:,:)
      integer(kind = kint), allocatable, target :: inod_psf(:)
!
      integer(kind = kint), allocatable, target :: iele_psf(:)
      integer(kind = kint), allocatable, target :: ie_psf(:,:)
      integer(kind = kint), allocatable, target :: ncomp_psf(:)
      integer(kind = kint), allocatable, target :: istack_comp_psf(:)
      character(len=kchara), allocatable, target :: psf_data_name(:)
!
      real(kind = kreal), allocatable, target :: rtp_psf(:,:)
      real(kind = kreal), allocatable, target :: ss_psf(:)
      real(kind = kreal), allocatable, target :: ar_psf(:)
      real(kind = kreal), allocatable, target :: as_psf(:)
!
      real(kind = kreal), allocatable :: ave_psf(:), rms_psf(:)
      real(kind = kreal), allocatable :: xmin_psf(:), xmax_psf(:)
!
      character(len=kchara) :: flag_psf
      integer(kind = kint) :: iflag_psf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_results
!
      allocate ( xx_psf(numnod_psf,3) )
      allocate ( inod_psf(numnod_psf) )
      allocate ( ie_psf(numele_psf,3) )
      allocate ( iele_psf(numele_psf) )
!
      allocate ( rtp_psf(numnod_psf,3) )
      allocate ( ss_psf(numnod_psf) )
      allocate ( ar_psf(numnod_psf) )
      allocate ( as_psf(numnod_psf) )
!
      if(numnod_psf .gt. 0) then
        xx_psf =  0.0d0
        rtp_psf = 0.0d0
        ss_psf =  0.0d0
        ar_psf =  0.0d0
        as_psf =  0.0d0
        inod_psf = 0
      end if
      if(numele_psf .gt. 0) then
        iele_psf =         0
        ie_psf =           0
      end if
!
      end subroutine allocate_psf_results
!
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_num_field
!
      allocate ( ncomp_psf(nfield_psf) )
      allocate ( istack_comp_psf(0:nfield_psf) )
      allocate ( psf_data_name(nfield_psf) )
!
      ncomp_psf =      -1
      istack_comp_psf = 0
!
      end subroutine allocate_psf_num_field
!
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_field_data
!
      allocate ( d_nod_psf(numnod_psf,ncomptot_psf) )
!
      allocate ( xmin_psf(ncomptot_psf) )
      allocate ( xmax_psf(ncomptot_psf) )
      allocate ( ave_psf(ncomptot_psf) )
      allocate ( rms_psf(ncomptot_psf) )
!
      d_nod_psf = 0.0d0
!
      xmin_psf = 1.0d30
      xmax_psf = 0.0d0
      ave_psf = 0.0d0
      rms_psf = 0.0d0
!
      end subroutine allocate_psf_field_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_stack_tot_psf_field
!
      integer(kind = kint) :: i
!
!
      istack_comp_psf(0) = 0
      do i = 1, nfield_psf
        istack_comp_psf(i) = istack_comp_psf(i-1) + ncomp_psf(i)
      end do
      ncomptot_psf = istack_comp_psf(nfield_psf)
!
      end subroutine count_stack_tot_psf_field
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_psf_results
!
      deallocate ( d_nod_psf )
      deallocate ( rtp_psf, ss_psf, ar_psf, as_psf )
      deallocate ( inod_psf, xx_psf )
      deallocate ( iele_psf, ie_psf )
      deallocate ( ncomp_psf )
      deallocate ( istack_comp_psf )
      deallocate ( psf_data_name )
!
      deallocate ( xmin_psf )
      deallocate ( xmax_psf )
      deallocate ( ave_psf )
      deallocate ( rms_psf )
!
      end subroutine deallocate_psf_results
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_psf_mesh_to_ucd_data(psf_ucd)
!
      use t_ucd_data
!
      type(ucd_data), intent(inout) :: psf_ucd
!
!
      call set_psf_mesh_to_ucd_mesh(psf_ucd)
      call set_psf_mesh_to_ucd_field(psf_ucd)
!
      end subroutine set_psf_mesh_to_ucd_data
!
!-----------------------------------------------------------------------
!
      subroutine set_psf_mesh_to_ucd_mesh(psf_ucd)
!
      use m_geometry_constants
      use t_ucd_data
      use set_ucd_data
!
      type(ucd_data), intent(inout) :: psf_ucd
!
!
      call link_node_data_2_output(numnod_psf, inod_psf, xx_psf,        &
     &    psf_ucd)
      call link_ele_data_2_output(numele_psf, num_triangle,             &
     &    iele_psf, ie_psf, psf_ucd)
!
      end subroutine set_psf_mesh_to_ucd_mesh
!
!-----------------------------------------------------------------------
!
      subroutine set_psf_mesh_to_ucd_field(psf_ucd)
!
      use t_ucd_data
      use set_ucd_data
!
      type(ucd_data), intent(inout) :: psf_ucd
!
!
      call link_field_data_2_output(numnod_psf,                         &
     &    nfield_psf, ncomptot_psf, nfield_psf, ncomptot_psf,           &
     &    ncomp_psf, psf_data_name, d_nod_psf, psf_ucd)
!
      psf_ucd%ifmt_file = iflag_psf_fmt
!
      end subroutine set_psf_mesh_to_ucd_field
!
!-----------------------------------------------------------------------
!
      end module  m_psf_results
