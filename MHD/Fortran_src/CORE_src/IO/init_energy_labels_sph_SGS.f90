!>@file   init_energy_labels_sph_SGS.f90
!!@brief  module init_energy_labels_sph_SGS
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Set energy labels for monitor output
!!
!!@verbatim
!!      subroutine init_energy_labels_w_filter(ene_labels)
!!        type(energy_label_param), intent(inout) :: ene_labels
!!@endverbatim
!!
      module init_energy_labels_sph_SGS
!
      use m_precision
!
      use t_energy_label_parameters
      use m_filtered_field_labels
!
      implicit none
!
!
      integer(kind = kint), parameter :: n_fil_ene = 4
      character(len=kchara), parameter :: ene_lebel_w_fil(4)            &
     &                    = (/'K_ene     ', 'M_ene     ',               &
     &                        'filter_KE ', 'filter_ME '/)
!
      private :: n_fil_ene, ene_lebel_w_fil
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_energy_labels_w_filter(ene_labels)
!
      use m_base_field_labels
      use m_filtered_field_labels
      use add_direction_labels
!
      type(energy_label_param), intent(inout) :: ene_labels
!
      integer(kind = kint) :: i
!
      ene_labels%n_fld_4_ene = n_fil_ene
      call alloc_energy_labels(ene_labels)
!
      ene_labels%field_name(1) = trim(velocity%name)
      ene_labels%field_name(2) = trim(magnetic_field%name)
      ene_labels%field_name(3) = trim(filter_velocity%name)
      ene_labels%field_name(4) = trim(filter_magne%name)
      do i = 1, n_fil_ene
        call add_vector_power_sph_label(ene_lebel_w_fil(i),             &
     &      ene_labels%label(1,i), ene_labels%label(2,i),               &
     &      ene_labels%label(3,i))
      end do
!
      end subroutine init_energy_labels_w_filter
!
! -----------------------------------------------------------------------
!
      end module init_energy_labels_sph_SGS

