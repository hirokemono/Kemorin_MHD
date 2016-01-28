!m_SGS_address.f90
!     module m_SGS_address
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!      subroutine set_SGS_addresses
!
      module m_SGS_address
!
      use m_precision
      use m_constants
!
      implicit  none
! 
      integer(kind=kint) :: ie_dvx = 0, ie_dfvx = 0
      integer(kind=kint) :: ie_dtx = 0, ie_dftx = 0
      integer(kind=kint) :: ie_dcx = 0, ie_dfcx = 0
      integer(kind=kint) :: ie_dbx = 0, ie_dfbx = 0
!
      integer (kind=kint) :: iak_sgs_hf =   izero
      integer (kind=kint) :: iak_sgs_mf =   izero
      integer (kind=kint) :: iak_sgs_cf =   izero
      integer (kind=kint) :: iak_sgs_lor =  izero
      integer (kind=kint) :: iak_sgs_uxb =  izero
      integer (kind=kint) :: iak_sgs_tbuo = izero
      integer (kind=kint) :: iak_sgs_cbuo = izero
!
      integer (kind=kint) :: iak_diff_v =  izero
      integer (kind=kint) :: iak_diff_t =  izero
      integer (kind=kint) :: iak_diff_b =  izero
      integer (kind=kint) :: iak_diff_c =  izero
!
      integer (kind=kint) :: iak_diff_hf =   izero
      integer (kind=kint) :: iak_diff_mf =   izero
      integer (kind=kint) :: iak_diff_cf =   izero
      integer (kind=kint) :: iak_diff_lor =  izero
      integer (kind=kint) :: iak_diff_uxb =  izero
!
!
      integer (kind=kint) :: icomp_sgs_hf =   izero
      integer (kind=kint) :: icomp_sgs_mf =   izero
      integer (kind=kint) :: icomp_sgs_cf =   izero
      integer (kind=kint) :: icomp_sgs_lor =  izero
      integer (kind=kint) :: icomp_sgs_uxb =  izero
      integer (kind=kint) :: icomp_sgs_tbuo = izero
      integer (kind=kint) :: icomp_sgs_cbuo = izero
!
      integer (kind=kint) :: icomp_diff_v =  izero
      integer (kind=kint) :: icomp_diff_t =  izero
      integer (kind=kint) :: icomp_diff_b =  izero
      integer (kind=kint) :: icomp_diff_c =  izero
!
      integer (kind=kint) :: icomp_diff_hf =   izero
      integer (kind=kint) :: icomp_diff_mf =   izero
      integer (kind=kint) :: icomp_diff_cf =   izero
      integer (kind=kint) :: icomp_diff_lor =  izero
      integer (kind=kint) :: icomp_diff_uxb =  izero
!
!  ------------------------------------------------------------------
!
      contains
!
!  ------------------------------------------------------------------
!
      subroutine set_SGS_addresses
!
      use m_control_parameter
      use m_int_vol_data
!
      integer(kind = kint) :: i
!
      i = 1
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (  iflag_SGS_heat .ne.      id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none ) then
         ie_dvx = i
         ie_dfvx = i + 9
         i = i + 18
        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         ie_dbx = i
         ie_dfbx = i + 9
         i = i + 18
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         ie_dbx = i
         ie_dfbx = i + 9
         i = i + 18
        end if
!
      else if (iflag_SGS_model .ne. id_SGS_none                         &
     &   .and. iflag_dynamic_SGS .eq. id_SGS_DYNAMIC_OFF) then
        if (   iflag_SGS_heat .ne.     id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none) then
         ie_dvx = i
         i = i + 9
        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         ie_dbx = i
         i = i + 9
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         ie_dbx = i
         i = i + 9
        end if
      end if
!
      end subroutine set_SGS_addresses
!
!  ------------------------------------------------------------------
!
      end module m_SGS_address
