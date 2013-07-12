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
!!      subroutine write_headers_psf_comp_name(id_file)
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
      if(numnod_psf .gt. 0) then
        xx_psf = 0.0d0
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
!
      subroutine write_headers_psf_comp_name(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: j, k
!
!
      write(id_file,'(a)',advance='no') ' psf_no, step_no, '
      do j = 1, nfield_psf
        if ( ncomp_psf(j) .eq. 1) then
          write(id_file,'(a,a2)',advance='no')                          &
     &                 trim( psf_data_name(j) ), ', '
        else
          do k = 1, ncomp_psf(j)
            write(id_file,1000,advance='no')                            &
     &                 trim( psf_data_name(j) ), k
          end do
        end if
      end do
!
 1000 format(a,'_',i1,', ')
!
      end subroutine write_headers_psf_comp_name
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
      use t_ucd_data
!
      type(ucd_data), intent(inout) :: psf_ucd
!
!
      psf_ucd%nnod = numnod_psf
      psf_ucd%nele = numele_psf
      psf_ucd%nnod_4_ele = 3
      psf_ucd%inod_global =>    inod_psf
      psf_ucd%xx =>             xx_psf
      psf_ucd%iele_global =>    iele_psf
      psf_ucd%ie =>             ie_psf
!
      end subroutine set_psf_mesh_to_ucd_mesh
!
!-----------------------------------------------------------------------
!
      subroutine set_psf_mesh_to_ucd_field(psf_ucd)
!
      use t_ucd_data
!
      type(ucd_data), intent(inout) :: psf_ucd
!
!
      psf_ucd%num_field = nfield_psf
      psf_ucd%ntot_comp = ncomptot_psf
      psf_ucd%num_comp =>       ncomp_psf
      psf_ucd%istack_comp =>    istack_comp_psf
      psf_ucd%phys_name =>      psf_data_name
      psf_ucd%d_ucd =>          d_nod_psf
!
      psf_ucd%itype_data_file = iflag_psf_fmt
!
      end subroutine set_psf_mesh_to_ucd_field
!
!-----------------------------------------------------------------------
!
      end module  m_psf_results
