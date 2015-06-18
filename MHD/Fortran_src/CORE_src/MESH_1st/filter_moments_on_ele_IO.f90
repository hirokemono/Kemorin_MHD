!
!     module filter_moments_on_ele_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine read_base_filter_info(id_file)
!      subroutine write_base_filter_info(id_file)
!
!      subroutine read_elength_ele(id_file)
!      subroutine write_elength_ele(id_file)
!      subroutine read_filter_moments_ele(id_file, ifil)
!      subroutine write_filter_moments_ele(id_file, ifil)
!
      module filter_moments_on_ele_IO
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_base_filter_info(id_file)
!
      use m_filter_elength
      use filter_mom_type_on_ele_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      call read_base_filter_info_type(id_file, FEM1_elen%filter_conf)
!
      end subroutine read_base_filter_info
!
!  ---------------------------------------------------------------------
!
      subroutine write_base_filter_info(id_file)
!
      use m_filter_elength
      use filter_mom_type_on_ele_IO
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call write_base_filter_info_type(id_file, FEM1_elen%filter_conf)
!
      end subroutine write_base_filter_info
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elength_ele(id_file)
!
      use m_filter_elength
      use filter_mom_type_on_ele_IO
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call read_elen_ele_type                                           &
     &   (id_file, FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
!
      end subroutine read_elength_ele
!
!  ---------------------------------------------------------------------
!
      subroutine write_elength_ele(id_file)
!
      use m_filter_elength
      use filter_mom_type_on_ele_IO
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call write_elen_ele_type                                          &
     &   (id_file, FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
!
      end subroutine write_elength_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moments_ele(id_file, ifil)
!
      use m_filter_moments
      use filter_mom_type_on_ele_IO
!
      integer(kind = kint), intent(in) :: id_file, ifil
!
!
      call read_filter_moms_ele_type                                    &
     &  (id_file, mom1%nele_fmom, mom1%mom_ele(ifil))
!
      end subroutine read_filter_moments_ele
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moments_ele(id_file, ifil)
!
      use m_filter_moments
      use filter_mom_type_on_ele_IO
!
      integer(kind = kint), intent(in) :: id_file, ifil
!
      call write_filter_moms_ele_type                                   &
     &  (id_file, mom1%nele_fmom, mom1%mom_ele(ifil))
!
      end subroutine write_filter_moments_ele
!
!  ---------------------------------------------------------------------
!
      end module filter_moments_on_ele_IO
