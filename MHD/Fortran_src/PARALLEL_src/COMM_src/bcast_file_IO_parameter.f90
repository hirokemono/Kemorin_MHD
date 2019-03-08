!> @file  bcast_file_IO_parameter.f90
!!      module bcast_file_IO_parameter
!!
!! @author  H. Matsui
!! @date Programmed in June, 2013
!
!> @brief Structure for file information (prefix, format, IO flag)
!!
!!@verbatim
!!      subroutine bcast_field_IO_parameter(file_param)
!!        type(field_IO_params), intent(inout) :: file_param
!!      subroutine bcast_FEM_file_IO_flags(FEM_mesh_flags)
!!        type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!!@endverbatim
!
      module bcast_file_IO_parameter
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_file_IO_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_field_IO_parameter(file_param)
!
      type(field_IO_params), intent(inout) :: file_param
!
!
      call MPI_Bcast(file_param%file_prefix, kchara,                    &
     &               CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(file_param%iflag_IO ,ione,                         &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(file_param%iflag_format, ione,                     &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_field_IO_parameter
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_FEM_file_IO_flags(FEM_mesh_flags)
!
      type(FEM_file_IO_flags), intent(inout) :: FEM_mesh_flags
!
!
      call MPI_Bcast(FEM_mesh_flags%iflag_access_FEM, ione,             &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(FEM_mesh_flags%iflag_output_SURF ,ione,            &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(FEM_mesh_flags%iflag_output_VMESH, ione,           &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_FEM_file_IO_flags
!
!  ---------------------------------------------------------------------
!
      end module bcast_file_IO_parameter
