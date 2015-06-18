!gz_filter_moments_ele_IO.f90
!     module gz_filter_moments_ele_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine read_base_filter_info_gz
!      subroutine write_base_filter_info_gz
!
!      subroutine read_elength_ele_gz
!      subroutine write_elength_ele_gz
!      subroutine read_filter_moments_ele_gz(ifil)
!      subroutine write_filter_moments_ele_gz(ifil)
!
      module gz_filter_moments_ele_IO
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
      subroutine read_base_filter_info_gz
!
      use m_filter_elength
      use gz_filter_mom_type_ele_IO
!
!
      call read_base_filter_info_type_gz(FEM1_elen%filter_conf)
!
      end subroutine read_base_filter_info_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_base_filter_info_gz
!
      use m_filter_elength
      use gz_filter_mom_type_ele_IO
!
!
      call write_base_filter_info_type_gz(FEM1_elen%filter_conf)
!
      end subroutine write_base_filter_info_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elength_ele_gz
!
      use m_filter_elength
      use gz_filter_mom_type_ele_IO
!
!
      call read_elen_ele_type_gz                                        &
     &   (FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
!
      end subroutine read_elength_ele_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_elength_ele_gz
!
      use m_filter_elength
      use gz_filter_mom_type_ele_IO
!
!
      call write_elen_ele_type_gz                                       &
     &   (FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
!
      end subroutine write_elength_ele_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moments_ele_gz(ifil)
!
      use m_filter_moments
      use gz_filter_mom_type_ele_IO
!
      integer(kind = kint), intent(in) :: ifil
!
!
      call read_filter_moms_ele_type_gz                                 &
     &   (mom1%nele_fmom, mom1%mom_ele(ifil))
!
      end subroutine read_filter_moments_ele_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moments_ele_gz(ifil)
!
      use m_filter_moments
      use gz_filter_mom_type_ele_IO
!
      integer(kind = kint), intent(in) :: ifil
!
!
      call write_filter_moms_ele_type_gz                                &
     &   (mom1%nele_fmom, mom1%mom_ele(ifil))
!
      end subroutine write_filter_moments_ele_gz
!
!  ---------------------------------------------------------------------
!
      end module gz_filter_moments_ele_IO
