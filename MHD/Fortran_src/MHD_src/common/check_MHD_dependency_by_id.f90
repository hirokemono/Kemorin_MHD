!>@file   check_MHD_dependency_by_id.f90
!!@brief  module check_MHD_dependency_by_id
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!!      subroutine check_dependencies_by_id(cd_prop, iphys, fld)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!
      module check_MHD_dependency_by_id
!
      use m_precision
      use m_error_IDs
      use m_phys_labels
!
      use calypso_mpi
!
      use t_physical_property
      use t_phys_address
      use t_phys_data
!
      implicit none
!
      private :: check_missing_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_dependencies_by_id(cd_prop, iphys, fld)
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: i, i_start
!
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. iphys%filter_fld%i_magne) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%base%i_magne, magnetic_field%name)
        else if(i_start .eq. iphys%i_filter_vecp) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%base%i_vecp, vector_potential%name)
        else if(i_start .eq. iphys%i_filter_comp) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%base%i_light, composition%name)
        end if
      end do
!
      end subroutine check_dependencies_by_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_missing_field(fld, iphys_tgt, iphys_ref, name)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: iphys_tgt, iphys_ref
      character(len = kchara), intent(in) :: name
!
      if(iphys_ref .gt. 0) return
      write(*,*) 'Following fields are required for ',                  &
     &     trim(field_name_by_address(fld, iphys_tgt)),                 &
     &     ': ', trim(name)
      call calypso_MPI_abort(ierr_fld,'Stop program.')
!
      end subroutine check_missing_field
!
! -----------------------------------------------------------------------
!
     end module check_MHD_dependency_by_id
