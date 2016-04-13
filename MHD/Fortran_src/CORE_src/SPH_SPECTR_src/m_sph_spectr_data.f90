!>@file   m_sph_spectr_data.f90
!!@brief  module m_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Flag and parameters for spherical transform dnyamo model
!!
!!
!!@verbatim
!!      subroutine allocate_reft_rj_data
!!      subroutine deallocate_reft_rj_data
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module m_sph_spectr_data
!
      use m_precision
      use t_phys_data
      use t_sph_spectr_data
!
      implicit  none
!
!>        Structure for field data
        type(phys_data), save :: rj_fld1
!
!>      Number of fields of scalar fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_scalar_rtp
!>      Number of fields of vector fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_vector_rtp
!>      Number of fields of tensor fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_tensor_rtp
!
!>    reference temerature spectr @f$ f(r,j) @f$
!!@verbatim
!!        reftemp_rj(kr,0) ... T_0
!!        reftemp_rj(kr,1) ... d T_0 / dr
!!@endverbatim
      real (kind=kreal), allocatable :: reftemp_rj(:,:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_reft_rj_data
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
      num = nidx_rj(1)
      allocate( reftemp_rj(num,0:1)   )
      reftemp_rj =  0.0d0
!
      end subroutine allocate_reft_rj_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine deallocate_reft_rj_data
!
      deallocate( reftemp_rj )
!
      end subroutine deallocate_reft_rj_data
!
!  --------------------------------------------------------------------
!
      end module m_sph_spectr_data
