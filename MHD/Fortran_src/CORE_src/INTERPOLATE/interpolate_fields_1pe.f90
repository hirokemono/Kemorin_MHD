!>@file   interpolate_fields_1pe.f90
!!@brief  module interpolate_fields_1pe
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  interpolation on each subdomains
!!
!!@verbatim
!!      subroutine itp_matvec_fields(np_smp, NP, NB, v_org,             &
!!     &          NC, NCM, INM, IAM, AM, NMAX_SUM, IEND_SUM_smp, vect)
!!@endverbatim
!
      module interpolate_fields_1pe
!
      use m_precision
!
      implicit none
!
      private :: itp_matvec_fields_8, itp_matvec_fields_20
      private :: itp_matvec_fields_27
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine itp_matvec_fields(np_smp, NP, NB, v_org,               &
     &          NC, NCM, INM, IAM, AM, NMAX_SUM, IEND_SUM_smp, vect)
!
      use m_geometry_constants
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, NB
      real(kind = kreal), intent(in) :: v_org(NB*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM, NMAX_SUM
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:4*np_smp)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NB*NC)
!
!
      if (NMAX_SUM .eq. num_t_linear)then
        call itp_matvec_fields_8(np_smp, NP, NB, v_org,                 &
     &      NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
      else if (NMAX_SUM .eq. num_t_quad)then
        call itp_matvec_fields_20(np_smp, NP, NB, v_org,                &
     &      NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
      else if (NMAX_SUM .eq. num_t_lag)then
        call itp_matvec_fields_27(np_smp, NP, NB, v_org,                &
     &      NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
      end if
!
      end subroutine itp_matvec_fields
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_fields_8(np_smp, NP, NB, v_org,             &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use interpolate_fields_ele8
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, NB
      real(kind = kreal), intent(in) :: v_org(NB*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:4*np_smp)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NB*NC)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call itp_matvec_fields_node(np_smp, NP, NB, v_org,                &
     &    NC, NCM, INM, IAM, IEND_SUM_smp(ist), vect)
!
      ist = np_smp
      call itp_matvec_fields_edge2(np_smp, NP, NB, v_org,               &
     &    NC, NCM, INM, IAM, AM, IEND_SUM_smp(ist), vect)
!
      ist = 2*np_smp
      call itp_matvec_fields_surf4(np_smp, NP, NB, v_org,               &
     &    NC, NCM, INM, IAM, AM, IEND_SUM_smp(ist), vect)
!
      ist = 3*np_smp
      call itp_matvec_fields_ele8(np_smp, NP, NB, v_org,                &
     &    NC, NCM, INM, IAM, AM, IEND_SUM_smp(ist), vect)
!
      end subroutine itp_matvec_fields_8
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_fields_20(np_smp, NP, NB, v_org,            &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use interpolate_fields_ele8
      use interpolate_fields_ele20
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, NB
      real(kind = kreal), intent(in) :: v_org(NB*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:4*np_smp)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NB*NC)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call itp_matvec_fields_node(np_smp, NP, NB, v_org,                &
     &    NC, NCM, INM, IAM, IEND_SUM_smp(ist), vect)
!
      ist = np_smp
      call itp_matvec_fields_edge3(np_smp, NP, NB, v_org,               &
     &    NC, NCM, INM, IAM, AM, IEND_SUM_smp(ist), vect)
!
      ist = 2*np_smp
      call itp_matvec_fields_ele8(np_smp, NP, NB, v_org,                &
     &    NC, NCM, INM, IAM, AM, IEND_SUM_smp(ist), vect)
!
      ist = 3*np_smp
      call itp_matvec_fields_ele20(np_smp, NP, NB, v_org,               &
     &    NC, NCM, INM, IAM, AM, IEND_SUM_smp(ist), vect)
!
      end subroutine itp_matvec_fields_20
!
! ----------------------------------------------------------------------
!
      subroutine itp_matvec_fields_27(np_smp, NP, NB, v_org,            &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp, vect)
!
      use interpolate_fields_ele8
      use interpolate_fields_ele20
      use interpolate_fields_ele27
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, NB
      real(kind = kreal), intent(in) :: v_org(NB*NP)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:4*np_smp)
      integer(kind = kint), intent(in) :: IAM(NCM)
      real(kind = kreal), intent(in) :: AM(NCM)
!
      real (kind=kreal), intent(inout) :: vect(NB*NC)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call itp_matvec_fields_node(np_smp, NP, NB, v_org,                &
     &    NC, NCM, INM, IAM, IEND_SUM_smp(ist), vect)
!
      ist = np_smp
      call itp_matvec_fields_edge3(np_smp, NP, NB, v_org,               &
     &    NC, NCM, INM, IAM, AM, IEND_SUM_smp(ist), vect)
!
      ist = 2*np_smp
      call itp_matvec_fields_surf9(np_smp, NP, NB, v_org,               &
     &    NC, NCM, INM, IAM, AM, IEND_SUM_smp(ist), vect)
!
      ist = 3*np_smp
      call itp_matvec_fields_ele27(np_smp, NP, NB, v_org,               &
     &    NC, NCM, INM, IAM, AM, IEND_SUM_smp(ist), vect)
!
      end subroutine itp_matvec_fields_27
!
! ----------------------------------------------------------------------
!
      end module interpolate_fields_1pe
