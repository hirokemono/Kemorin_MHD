!>@file   t_phys_name_4_sph_trans.f90
!!@brief  module t_phys_name_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2008
!
!!@verbatim
!!      subroutine copy_sph_trans_nums_from_rtp(fld_rtp)
!!      subroutine copy_sph_name_rj_to_rtp(rj_fld, fld_rtp)
!!        type(phys_data), intent(in) :: rj_fld
!!@endverbatim
!
      module t_phys_name_4_sph_trans
!
      use m_precision
!
      implicit  none
!
      type field_name_4_sph_trans
!>        total number of components for spherical harmonics transform
        integer(kind = kint) :: ncomp_trans
!>        total number of scalars for spherical harmonics transform
        integer(kind = kint) :: nscalar_trans
!
!>        Number of fields on spherical grid @f$ f(r,\theta,\phi) @f$
        integer (kind=kint) :: nfld
!>        Field name for @f$ f(r,\theta,\phi) @f$
        character (len=kchara), allocatable :: fld_name(:)
!
!>        Number of fields of scalar fields @f$ f(r,\theta,\phi) @f$
        integer (kind=kint) :: num_scalar
!>        Number of fields of vector fields @f$ f(r,\theta,\phi) @f$
        integer (kind=kint) :: num_vector
!>        Number of fields of tensor fields @f$ f(r,\theta,\phi) @f$
        integer (kind=kint) :: num_tensor
!
!>        Start field address of scalar fields @f$ f(r,\theta,\phi) @f$
        integer (kind=kint) :: istart_scalar_rtp
!>        Start field address of vector fields @f$ f(r,\theta,\phi) @f$
        integer (kind=kint) :: istart_vector_rtp
!>        Start field address of tensor fields @f$ f(r,\theta,\phi) @f$
        integer (kind=kint) :: istart_tensor_rtp
      end type field_name_4_sph_trans
!
      private :: allocate_phys_rtp_name
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_phys_rtp_name(fld_rtp)
!
      type(field_name_4_sph_trans), intent(inout) :: fld_rtp
!
!
      allocate(fld_rtp%fld_name(fld_rtp%nfld) )
!
      end subroutine allocate_phys_rtp_name
!
!  --------------------------------------------------------------------
!
      subroutine deallocate_phys_rtp_name(fld_rtp)
!
      type(field_name_4_sph_trans), intent(inout) :: fld_rtp
!
!
      deallocate( fld_rtp%fld_name )
!
      end subroutine deallocate_phys_rtp_name
!
!  --------------------------------------------------------------------
!
      subroutine copy_sph_trans_nums_from_rtp(fld_rtp)
!
      use m_machine_parameter
      use m_phys_constants
!
      type(field_name_4_sph_trans), intent(inout) :: fld_rtp
!
!
      fld_rtp%ncomp_trans =  fld_rtp%num_tensor * n_sym_tensor          &
     &                     + fld_rtp%num_vector * n_vector              &
     &                     + fld_rtp%num_scalar * n_scalar
      fld_rtp%nscalar_trans =  fld_rtp%num_tensor * n_sym_tensor        &
     &                       + fld_rtp%num_scalar * n_scalar
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans', fld_rtp%ncomp_trans
        write(*,*) 'nscalar_trans',   fld_rtp%nscalar_trans
      end if
!
      end subroutine copy_sph_trans_nums_from_rtp
!
! -----------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine copy_sph_name_rj_to_rtp(rj_fld, fld_rtp)
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_phys_data
!
      type(phys_data), intent(in) :: rj_fld
      type(field_name_4_sph_trans), intent(inout) :: fld_rtp
!
!
      integer(kind = kint) :: i, i0
!
      fld_rtp%nfld =  rj_fld%num_phys
      call allocate_phys_rtp_name(fld_rtp)
!
      i0 = 0
      fld_rtp%num_scalar = 0
      do i = 1, rj_fld%num_phys
        if (rj_fld%num_component(i) .eq. n_scalar) then
          i0 = i0 + 1
          fld_rtp%num_scalar = fld_rtp%num_scalar + 1
          fld_rtp%fld_name(i0) =        rj_fld%phys_name(i)
        end if
      end do
!
      fld_rtp%num_vector = 0
      do i = 1, rj_fld%num_phys
        if (rj_fld%num_component(i) .eq. n_vector) then
          i0 = i0 + 1
          fld_rtp%num_vector = fld_rtp%num_vector + 1
          fld_rtp%fld_name(i0) =        rj_fld%phys_name(i)
        end if
      end do
!
      fld_rtp%num_tensor = 0
      do i = 1, rj_fld%num_phys
        if (rj_fld%num_component(i) .eq. n_sym_tensor) then
          i0 = i0 + 1
          fld_rtp%num_tensor = fld_rtp%num_tensor + 1
          fld_rtp%fld_name(i0) =        rj_fld%phys_name(i)
        end if
      end do
!
      fld_rtp%istart_scalar_rtp = 1
      fld_rtp%istart_vector_rtp = fld_rtp%istart_scalar_rtp             &
     &                            + fld_rtp%num_scalar
      fld_rtp%istart_tensor_rtp = fld_rtp%istart_vector_rtp             &
     &                            + fld_rtp%num_vector
!
      if (iflag_debug .gt. 0) then
!        write(*,*) 'num_phys_rj', rj_fld%num_phys
!        write(*,*) 'id, components, stack, phys_name_rj'
!        do i = 1, rj_fld%num_phys
!          write(*,*) i, rj_fld%num_component(i),                       &
!     &        rj_fld%istack_component(i), trim(rj_fld%phys_name(i))
!        end do
        write(*,*)
        write(*,*) 'num_phys_rtp', fld_rtp%nfld
        write(*,*) 'phys_name_rtp', size(fld_rtp%fld_name)
        write(*,*) 'id, components, stack, fld_rtp%fld_name'
        do i = 1, fld_rtp%nfld
          write(*,*) i, trim(fld_rtp%fld_name(i))
        end do
        write(*,*) 'istart_scalar_rtp',                                 &
     &              fld_rtp%istart_scalar_rtp, fld_rtp%num_scalar
        write(*,*) 'istart_vector_rtp',                                 &
     &              fld_rtp%istart_vector_rtp, fld_rtp%num_vector
        write(*,*) 'istart_tensor_rtp',                                 &
     &              fld_rtp%istart_tensor_rtp, fld_rtp%num_tensor
      end if
!
!
      end subroutine copy_sph_name_rj_to_rtp
!
! -------------------------------------------------------------------
!
      end module t_phys_name_4_sph_trans
 